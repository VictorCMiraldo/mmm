{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  MMM.OOP.Language.Syntax
-- Copyright   :  HASLAb team, University of Minho
-- License     :  MIT
--
-- Maintainer  :  Victor Miraldo <victor.cacciari@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
--We provide the AST for a minimal object-oriented language. This language
--is used to conduct experiments using mealy-machines for it's semantics.
----------------------------------------------------------------------------
module MMM.OOP.Language.Syntax where

import MMM.Util.Pretty

import Data.Data
import Data.Typeable
import Data.Generics.Uniplate.Data(universeBi, transformBi)

import Text.Parsec.Pos

--------------------------------------------------------------------------------
-- * Identifiers

-- The AST is parametrized on identifiers, this is the basic one where we annotate
-- the source position of the variable.

-- |Stores the name and source position of an identifier.
data Var
  = SimpleVar SourcePos String
  | ThisVar   Var
  | Access    String Var
  deriving (Show, Data, Typeable)
  
voidLoc :: SourcePos
voidLoc = newPos "<void>" 0 0
  
instance Pretty Var where
  pretty (SimpleVar _ s) = t s
  pretty (ThisVar s)   = t "this" <> t "." <> pretty s
  pretty (Access s v)    = pretty v <> t "." <> pretty s 
  
instance Ord Var where
  compare a b = (show $ pretty a) `compare` (show $ pretty b)
  
instance Eq Var where
  v1 == v2 = (varName v1) == (varName v2)
  
class (Show a, Eq a, Ord a, Data.Data.Data a, Typeable a, Pretty a) => IsVar a where
  varName       :: a -> String
  varIsPure     :: a -> Bool
  varAccessList :: a -> [String]
  varLoc        :: a -> (Int, String)
  varAppName    :: String -> a -> a
  varBuild      :: String -> a
  
instance IsVar Var where
  varName (SimpleVar _ s) = s
  varName (ThisVar v)     = varName v
  varName (Access s v)    = varName v ++ "_" ++ s
  
  varIsPure (SimpleVar _ _) = True
  varIsPure (ThisVar _)     = False
  varIsPure (Access _ v)    = varIsPure v
  
  varAccessList (SimpleVar _ s) = [s]
  varAccessList (ThisVar s) = "this" : varAccessList s
  varAccessList (Access s v) = varAccessList v ++ [s]
  
  varLoc (SimpleVar p _) = (sourceLine p, sourceName p)
  varLoc (ThisVar v)     = varLoc v
  varLoc (Access _ v)    = varLoc v
  
  varAppName s (SimpleVar p vn) = SimpleVar p (vn ++ s)
  varAppName s (ThisVar v)      = ThisVar $ varAppName s v
  varAppName s (Access s' v)    = Access s' $ varAppName s' v
  
  varBuild s = SimpleVar voidLoc s
  
instance IsVar String where
  varName = id
  
  varIsPure = not . ('.' `elem`)
  
  varAccessList = (:[])
  
  varLoc _ = (0, "<loc_info_unavailable>")
  
  varAppName = (++)
  
  varBuild = id

  
--------------------------------------------------------------------------------
-- * Types
  
-- |The structured types.
data Type id
  = Simple TySimple
  | Object id
  | ListOf (Type id)
  deriving (Eq, Show, Data, Typeable, Functor)
  
instance Pretty id => Pretty (Type id) where
  pretty (Simple ts) = pretty ts
  pretty (Object ob) = pretty ob
  pretty (ListOf ts) = t "list" <+> t "of" <+> pretty ts 
  
-- |Atomic types
data TySimple
  = TyInt
  | TyFloat
  | TyVoid
  | TyBool
  deriving (Eq, Show, Data, Typeable)
  
instance Pretty TySimple where
  pretty TyInt   = t "int"
  pretty TyFloat = t "float"
  pretty TyVoid  = t "void"
  pretty TyBool  = t "bool"
  
--------------------------------------------------------------------------------
-- * Modules

data Module id = Module
  { moduleImport :: [String]
  , moduleDecls  :: [Class id]
  }
  
instance Pretty id => Pretty (Module id) where
  pretty (Module is cs)
    =   vcat (map ((<> semi) . (t "import" <+>) . pretty) is)
    $+$ vcat (map pretty cs) 
  
--------------------------------------------------------------------------------
-- * Classes 

-- |A Class is declared with a name and a possible inheritance clause,
--  it must be accompained with it's definition.
data Class id = Class 
  { className     :: String
  , classInherits :: (Maybe (Class id)) 
  , classDef      :: (ClassDef id)
  } deriving (Show, Data, Typeable, Functor)
  
instance Pretty id => Pretty (Class id) where
  pretty (Class s exts def)
    = t "class" <+> t s <+> blocked def 

-- |Class Definition   
data ClassDef id = ClassDef
  { classDefVars  :: [VarDef id]
  , classDefFuncs :: [MethodDef id]
  } deriving (Show, Data, Typeable, Functor)
  
instance Pretty id => Pretty (ClassDef id) where
  pretty (ClassDef vs ms)
    = vcat $ map pretty vs ++ map pretty ms
  
--------------------------------------------------------------------------------
-- ** Definitions  

-- |A variable definition does not have any visibility constraint (private, public)
--  but must be accompained with a type.
data VarDef id
  = VarDef id (Type id)
  deriving (Show, Data, Typeable, Functor)
  
vardefName :: VarDef id -> id
vardefName (VarDef i _) = i
  
instance Pretty id => Pretty (VarDef id) where
  pretty (VarDef s ty) = t "var" <+> pretty s <+> colon <+> pretty ty <> t ";"
   
-- |A method is a simple function.
data MethodDef id = Method 
  { methodName  :: String
  , methodParms :: [(id, Type id)] 
  , methodRet   :: (Type id) 
  , methodBody  :: (StmtSeq id)
  } deriving (Show, Data, Typeable, Functor)
  
instance Pretty id => Pretty (MethodDef id) where
  pretty (Method s parms ret sts)
    = t s <+> parens (map pParms parms `sepBy` comma) 
          <+> colon <+> pretty ret 
          <+> blocked sts
    where
      pParms (i, t) = pretty i <+> colon <+> pretty t
          
--------------------------------------------------------------------------------
-- * Statements And Expressions   
  
type StmtSeq id = [Stmt id]

instance Pretty id => Pretty (StmtSeq id) where
  pretty sts = vcat (map pretty sts)
  
-- |The 'Stmt' type id as minimal as possible.
data Stmt id
  = StmtAssign (Lhs id) (Exp id)
  | StmtIf (Exp id) (StmtSeq id) (StmtSeq id)
  | StmtCall id [Exp id]
  | StmtRet  (Exp id)
  -- Auxiliar constructors
  | StmtBlock [Stmt id] -- blocks for easier substitution
  | StmtPhi id id id id -- Phi functions are used to contract names after SSA.
  | StmtSet id id       -- gets and sets lifted
  | StmtGet id id
  | StmtCollect [id]    -- garbage collection
  deriving (Show, Data, Typeable, Functor)
  
instance Pretty id => Pretty (Stmt id) where
  pretty (StmtAssign lhs exp) = pretty lhs <+> t "=" <+> pretty exp <> semi
  pretty (StmtCall f ps)      = pretty f <> parens (map pretty ps `sepBy` comma) <> semi
  pretty (StmtRet e)          = t "return" <+> pretty e <> semi
  pretty (StmtIf c th e)      = t "if" <+> pretty c 
                            $+$ t "then" <+> blocked th
                            $+$ if length e == 0 then empty else t "else" <+> blocked e
  pretty (StmtPhi nv c ov nov)
    = pretty nv <+> t "=" <+> t "\\phi" <> parens (map pretty [c, ov, nov] `sepBy` comma)
  pretty (StmtSet i v)
    = t "SET" <> parens (pretty i <> comma <+> pretty v) <> semi
  pretty (StmtGet i v)
    = t "GET" <> parens (pretty i <> comma <+> pretty v) <> semi
  pretty (StmtBlock s)
    = vcat $ map pretty s
  pretty (StmtCollect v)
    = t "COLLECT" <> parens (map pretty v `sepBy` comma)
                            
-- |Remove the blocks from a StmtSeq
stmtRmvBlocks :: StmtSeq id -> StmtSeq id
stmtRmvBlocks = concatMap aux
  where
    aux (StmtBlock sts) = sts
    aux s               = [s]
 
                            
-- |Left-hand side can be simple identifiers or state variables. 
data Lhs id
  = LhsVar    id
  deriving (Show, Data, Typeable, Functor)
  
instance Pretty id => Pretty (Lhs id) where
  pretty (LhsVar i)   = pretty i
  
-- |Expressions are standard.
data Exp id
  = ExpLit   Literal
  | ExpVar   id
  | ExpCall  id [Exp id]
  | ExpBinop Binop (Exp id) (Exp id)
  | ExpUnop  Unop  (Exp id)
  | ExpCond  id (Exp id) (Exp id)
  | ExpBtin  ExpBuiltin [Exp id]
  | ExpUndefined
  deriving (Eq, Show, Data, Typeable, Functor)
  
instance Pretty id => Pretty (Exp id) where
  pretty (ExpLit lit) = pretty lit
  pretty (ExpVar i)   = pretty i
  pretty (ExpCall i ps) = pretty i <> parens (map pretty ps `sepBy` comma)
  pretty (ExpBinop o m n) = parens $ pretty m <+> pretty o <+> pretty n
  pretty (ExpUnop o m)    = parens $ pretty o <+> pretty m
  pretty (ExpCond c th el)= parens $ pretty c <+> t "?" <+> pretty th <+> t ":" <+> pretty el
  pretty ExpUndefined     = t "undefined"
  pretty (ExpBtin op ps)  = parens $ pretty op <+> hsep (map pretty ps)
  
varsOfExp :: (IsVar id) => Exp id -> [id]
varsOfExp e = [ v | (ExpVar v) <- universeBi e ]

expIsLinear :: Exp id -> Bool
expIsLinear (ExpVar _) = True
expIsLinear (ExpLit _) = True
expIsLinear (ExpCall _ _) = True
expIsLinear _          = False

expIsLit :: Exp id -> Bool
expIsLit (ExpLit _) = True
expIsLit _          = False

data ExpBuiltin
  = ExpBHead
  | ExpBTail
  | ExpBCons
  | ExpBIn
  | ExpBLength
  | ExpBCat
  | ExpBFail
  deriving (Eq, Show, Data, Typeable)
  
instance Pretty ExpBuiltin where
  pretty ExpBHead = t "head"
  pretty ExpBTail = t "tail"
  pretty ExpBFail = t "error" <+> t "\"panic\""
  pretty ExpBCons = t "(:)"
  pretty ExpBCat  = t "(++)"
  pretty ExpBIn   = t "elem"
  pretty ExpBLength = t "length"
  
data Literal
  = LitInt   Int
  | LitFloat Float
  | LitBool  Bool
  | LitList  [Literal]
  deriving (Eq, Show, Data, Typeable)
  
instance Pretty Literal where
  pretty (LitInt i)   = pretty i
  pretty (LitFloat f) = t $ show f
  pretty (LitBool b)  = t $ show b
  pretty (LitList l)  = brackets (map pretty l `sepBy` comma)

data Binop
  -- arithmetic
  = Sum | Mul | Sub | Div | Exp | Mod
  -- comparisson
  | Eq | Leq | Le | Ge | Geq | Neq
  -- boolean
  | And | Or
  deriving (Eq, Show, Data, Typeable)
  
instance Pretty Binop where
  pretty Sum = t "+"
  pretty Mul = t "*"
  pretty Exp = t "^"
  pretty Sub = t "-"
  pretty Div = t "/"
  pretty Mod = t "`mod`"
  pretty Eq  = t "=="
  pretty Neq = t "/="
  pretty Leq = t "<="
  pretty Geq = t ">="
  pretty Ge  = t ">"
  pretty And = t "&&"
  pretty Or  = t "||"
  
data Unop
  = Not
  | Inv
  deriving (Eq, Show, Data, Typeable)

instance Pretty Unop where
  pretty Not = t "not"
  pretty Inv = t "0" <+> t "-"
  
