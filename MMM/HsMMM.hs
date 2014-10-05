{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
module MMM.HsMMM where

import MMM.Core.FuncComb((^^^), dirac, grd)
import MMM.OOP.Language.Syntax as OOP

import MMM.Util.Pretty

import qualified Data.Map as M
import Control.Monad.State

--------------------------------------------------------------------------------
-- * Embeded Haskell with MMM support.

--------------------------------------
-- ** Types

data HsTyAtom
  = HsTyMMM  HsTy HsTy HsTy HsTy
  | HsTyVar  String
  | HsTyInt
  | HsTyFloat
  | HsTyBool
  deriving (Eq, Show)
  
instance Pretty HsTyAtom where
  pretty HsTyInt = t "Integer"
  pretty HsTyFloat = t "Float"
  pretty HsTyBool = t "Bool"
  pretty (HsTyVar id) = pretty id
  pretty (HsTyMMM m s i o) = t "MMM" <+> pretty m <+> pretty s <+> pretty i <+> pretty o

data HsTy
  = HsTyNil
  | HsTy1    HsTyAtom
  | HsTyList HsTy
  | HsTyProd [HsTy]
  | HsTySum  [HsTy]
  | HsTyFun  HsTy HsTy
  | HsTyApp  HsTy HsTy
  | HsTyCtx  HsTy HsTy
  deriving (Eq, Show)
  
instance Pretty HsTy where
  pretty (HsTy1 ty)    = pretty ty
  pretty (HsTyNil)     = t "()"
  pretty (HsTyList l)  = brackets (pretty l)
  pretty (HsTyProd l)  = pprAssocLeftWith (\d a -> parens $ d <> comma <+> pretty a) l
  pretty (HsTySum l)   = pprAssocLeftWith (\d a -> t "Either" <+> parens d <+> parens (pretty a)) l
  pretty (HsTyFun a b) = pretty a <+> t "->" <+> parens (pretty b)
  pretty (HsTyCtx a b) = pretty a <+> t "=>" <+> parens (pretty b)
  pretty (HsTyApp a b) = parens $ pretty a <+> pretty b
  
hstyFromType :: (IsVar id) => OOP.Type id -> HsTy
hstyFromType (OOP.Simple OOP.TyVoid) = HsTyNil
hstyFromType (OOP.Simple ts) = HsTy1 $ hstyatomFromTySimple ts
hstyFromType (OOP.Object id) = HsTy1 $ HsTyVar (varName id)
hstyFromType (OOP.ListOf a)  = HsTyList $ hstyFromType a

hstyatomFromTySimple :: OOP.TySimple -> HsTyAtom
hstyatomFromTySimple TyInt = HsTyInt
hstyatomFromTySimple TyFloat = HsTyFloat
hstyatomFromTySimple TyBool = HsTyBool

hstyCtxStrongVar :: String -> HsTy -> HsTy
hstyCtxStrongVar s = HsTyCtx (HsTyApp (HsTy1 $ HsTyVar "Strong") (HsTy1 $ HsTyVar s))
  
--------------------------------------
-- ** Top-level declarations

type HsModule = [HsStmt]

hsmoduleGetName :: HsModule -> String
hsmoduleGetName hsm
  = let
    l = grd (==[]) $ filter isSModDecl hsm
  in either (const "DEFAULT") (\((HsSModDecl s):_) -> s) l
  where
    isSModDecl (HsSModDecl _) = True
    isSModDecl _              = False 

data HsStmt
  = HsSDecl   HsDecl
  | HsSTyDecl String HsTy
  | HsSPragma HsPragma
  | HsSImport String (Maybe String)
  | HsSModDecl String
  deriving (Eq, Show)
  
instance Ord HsStmt where
  compare (HsSModDecl _) _                = GT
  compare (HsSImport _ _) (HsSModDecl _)  = LT
  compare (HsSImport _ _) _               = GT
  compare (HsSTyDecl _ _) (HsSModDecl _)  = LT
  compare (HsSTyDecl _ _) (HsSImport _ _) = LT
  compare (HsSTyDecl _ _) _               = GT
  compare _ _                             = EQ
  
  
hsStmtIsLineP :: HsStmt -> Bool
hsStmtIsLineP (HsSPragma p) = hspragmaIsLine p
hsStmtIsLineP _             = False
  
instance Pretty HsStmt where
  pretty (HsSDecl d)   = pretty d
  pretty (HsSPragma p) = pretty p
  pretty (HsSTyDecl s ty) 
    = t "type" <+> t s <+> t "=" <+> pretty ty
  pretty (HsSImport s Nothing)
    = t "import" <+> t s 
  pretty (HsSImport s (Just q))
    = t "import" <+> t "qualified" <+> t s <+> t "as" <+> t q
  pretty (HsSModDecl s)
    = t "module" <+> t s <+> t "where" <+> t "\n\n"
    
data HsPragma
  = HsPLine Int String
  deriving (Eq, Show)
  
hspragmaIsLine :: HsPragma -> Bool
hspragmaIsLine (HsPLine _ _) = True
  
instance Pretty HsPragma where
  pretty (HsPLine i f) = t "{-#" <+> t "LINE" <+> pretty i <+> t "\"" <> t f <> t "\"" <+> t "#-}"

data HsDecl 
  = HsDecl
      { hsDeclName :: String
      , hsDeclType :: Maybe HsTy
      , hsDeclDef  :: HsExp 
      , hsDeclAux  :: [HsDecl]
      } 
  | HsDeclComment HsDecl String
  deriving (Eq, Show)
  
instance Pretty HsDecl where
  pretty (HsDeclComment d c) 
    = comment c $+$ pretty d
    where
      comment c = vcat (map ((t "--" <+>) . pretty) $ lines c)
      
  pretty (HsDecl n ty d auxs)   
    = maybe empty ((t n <+>) . (t "::" <+>) . pretty) ty 
    $+$ t n <+> t "=" <+> pretty d
    $+$ case auxs of
            [] -> empty
            _  -> nest 2 (t "where" $+$ nest 2 (vcat . map pretty $ auxs))
    $+$ t "\n"
            
hsdeclSimpl :: String -> HsExp -> HsDecl
hsdeclSimpl s e = HsDecl s Nothing e []

hsdeclSimplTy :: String -> HsTy -> HsExp -> HsDecl
hsdeclSimplTy s ty e = HsDecl s (Just ty) e []
   
--------------------------------------
-- ** Expressions

data HsExp
  = HsEId
  | HsEEta
  | HsEBang   
  | HsEMMM   HsMMM
  | HsEVar   String
  | HsEProj  Int Int 
  | HsEInj   Int Int
  | HsESplit HsExp HsExp
  | HsEEith  HsExp HsExp
  | HsEProd  [HsExp]
  | HsESum   [HsExp]
  | HsEComp  HsExp HsExp
  | HsEKlei  HsExp HsExp
  | HsEOOP   (OOP.Exp String)
  | HsELam   [String] HsExp
  deriving (Eq, Show)
  
instance Pretty HsExp where
  pretty HsEBang        = t "bang"
  pretty HsEId          = t "id"
  pretty HsEEta         = t "return"
  pretty (HsEMMM m)     = pretty m
  pretty (HsEVar v)     = pretty v
  pretty (HsEProj 1 _)  = t "id"
  pretty (HsEProj 2 i)  = t "p" <> pretty i
  pretty (HsEProj c i)  = pretty $ buildBinary c i (HsEProj 2)
  pretty (HsEInj 2 i)   = t "i" <> pretty i  
  pretty (HsEInj c i)   = pretty $ buildBinary c i (HsEInj 2)
  pretty (HsESplit a b) = parens $ t "split" <+> pretty a <+> pretty b
  pretty (HsEEith a b)  = parens $ t "either" <+> pretty a <+> pretty b
  pretty (HsEProd as)   = pprAssocLeftWith (\r a -> parens $ r <+> t "><" <+> pretty a) as
  pretty (HsESum as)    = pprAssocLeftWith (\r a -> parens $ r <+> t "-|-" <+> pretty a) as
  pretty (HsEComp f g)  = parens $ pretty f <+> t "." <+> pretty g
  pretty (HsEKlei m n)  = parens $ pretty m <+> t ".!" <+> pretty n
  pretty (HsEOOP e)     = pretty e 
  pretty (HsELam [] e)  = parens $ t "const" <+> pretty e
  pretty (HsELam v e)   
    = parens $ t "\\" <+> (pprAssocLeftWith (\r a -> parens $ r <+> t "," <+> pretty a) v)
                      <+> t "->"
                      <+> pretty e
                                          
buildBinary :: Int -> Int -> (Int -> HsExp) -> HsExp
buildBinary max 1 f = foldl1 HsEComp $ replicate (max - 1) (f 1)
buildBinary max i f 
  | max > i = let aux = foldl1 HsEComp $ replicate (max - i) (f 1)
              in HsEComp (f 2) aux
  | otherwise = f 2
  
  
hsexpLiftMMM :: HsExp -> HsExp
hsexpLiftMMM = HsEMMM . HsMLift

--------------------------------------
-- ** Mealy Machines

data HsMMM
  = HsMLift  HsExp
  | HsMVar   String
  | HsMExtL  HsMMM
  | HsMExtR  HsMMM
  | HsMRunm  HsMMM
  | HsMRunmI HsMMM
  | HsMRunm1 HsMMM
  | HsMKlei  HsMMM HsMMM
  | HsMCond  HsExp HsMMM HsMMM
  | HsMGetSt
  | HsMCopy
  deriving (Eq, Show)
  
instance Pretty HsMMM where
  pretty (HsMLift f)  = parens $ t "f2m" <+> pretty f
  pretty (HsMVar v)   = pretty v
  pretty (HsMExtL m)  = parens $ t "extl" <+> pretty m
  pretty (HsMExtR m)  = parens $ t "extr" <+> pretty m
  pretty (HsMRunm m)  = parens $ t "runm" <+> pretty m
  pretty (HsMRunmI m) = parens $ t "runm_" <+> pretty m
  pretty (HsMRunm1 m)  = parens $ t "runm1" <+> pretty m
  pretty (HsMKlei m n)= pretty m $+$ t ".!" <+> pretty n
  pretty (HsMCond c m n) = parens $ t "mcond" $+$ pretty c 
                                  $+$ parens (pretty m) 
                                  $+$ parens (pretty n)
  pretty HsMGetSt     = t "getst"
  pretty HsMCopy      = t "copy"
  
