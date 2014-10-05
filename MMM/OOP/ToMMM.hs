{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ToMMM
-- Copyright   :  HASLAb team, University of Minho
-- License     :  MIT
--
-- Maintainer  :  Victor Miraldo <victor.cacciari@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
--Bridges 'MMM.OOP.Language.Syntax' to 'MMM.HsMMM', translating a OOP module
--to a Haskell MMM module.
----------------------------------------------------------------------------
module MMM.OOP.ToMMM where

import MMM.Core.FuncComb

import MMM.OOP.Language.Syntax
import MMM.OOP.Language.Utils
import MMM.OOP.SSA
import MMM.OOP.OrdVarSet
import MMM.OOP.ObjSt

import MMM.HsMMM

import MMM.Util.Util
import MMM.Util.Pretty

import Control.Monad.State

import Data.List(cycle, sort)
import Data.Char(toLower)

#ifdef __DEBUG__
import qualified Debug.Trace as T
#endif

--------------------------------------------------------------------------------
-- * OOP to MMM translation

-- ** Synonyms

l2 :: (MonadTrans t, MonadTrans u, Monad m
      ,Monad (u m)) => m a -> t (u m) a
l2 = lift . lift

type OME m = OM (E m)
type OrdVSOME id m = OrdVS id (OM (E m))


-- |Default import statements
defImports :: String -> HsModule
defImports classname = [ HsSModDecl classname
                       , HsSImport "MMM.Core.All"      Nothing
                       , HsSImport "Data.Default(def)" Nothing]

-- |Translates a module
trModule :: (IsVar id, MyMonad m) => Module id -> E m HsModule
trModule (Module is cs)
  = let
    mis = defImports $ className $ head cs
    iis = map (flip HsSImport Nothing) is
  in do
    mc <- mapM classToModule cs
    return $ mis ++ iis ++ sort (concat mc)

-- |Translates a class into a HsModule
classToModule :: (IsVar id, MyMonad m) => Class id -> E m HsModule
classToModule  c
  = let
    cvars = classDefVars $ classDef c
    cfuns = classDefFuncs $ classDef c
    pair a b = (a, b)
  in do
    -- returns the getters and setters for our class
    (ty, getset) <- runObjSt c (pair <$> mkTyDecl <*> mkGetsSets cvars)
    
    -- translates our methods
    ms     <- runObjSt c (mapM methodToMMM cfuns)
    return $ ty:getset ++ concat ms
    
-- |Builds the getters and setters for a given object.   
mkGetsSets :: (IsVar id, MyMonad m) => [VarDef id] -> OME m HsModule
mkGetsSets l
  = do
    -- get's the name and location of variables
    let (l', lps) = unzip $ map (split varName varLoc . vardefName) l
    -- build the "line" pragmas for better error messages.
    let lps'      = map (HsSPragma . uncurry HsPLine) lps
    -- builds the pi_x for each x in l
    pixs <- mapM omPiX l'
    -- prepends them with p1, for the setters.
    let pixsp1 = map (flip HsEComp (HsEProj 2 1)) pixs
    let lidx   = zip l' [0..]
    sets <- mapM (buildSetDecl . buildSetter pixsp1) lidx 
    gets <- mapM (buildGetDecl . buildGetter pixs) lidx
    -- adds the line pragmas before their respective getters and setters.
    return $ intersperse' (cycle lps') (sets ++ gets)    
  where
    replace i a as
      = let
        (bas, _:aas) = splitAt i as
      in bas ++ a:aas
  
    buildSetter pixp1 (v, i)
      = let
        (h:t) = replace i (HsEProj 2 2) pixp1
      in (HsEComp HsEEta (HsESplit (foldl HsESplit h t) HsEBang), v)

    buildGetter pixs (v, i)
      = (HsEKlei (HsEMMM $ HsMLift (pixs !! i)) (HsEMMM HsMGetSt), v)
     
    buildGetDecl (hsexp, name)
      = do
        ty <- omGetStType name 
        nm <- omGetSt name
        return $ HsSDecl $ hsdeclSimplTy nm ty hsexp
    
    buildSetDecl (hsexp, name)
      = do
        ty <- omSetStType name 
        nm <- omSetSt name
        return $ HsSDecl $ hsdeclSimplTy nm ty hsexp
        
mkTyDecl :: (MyMonad m) => OME m HsStmt
mkTyDecl = HsSTyDecl <$> omObjName <*> (HsTyProd <$> omObjType)
        
--------------------------------------------------------------------------------
--

type WS m = WriterT [HsDecl] (StateT Int m)

runWS :: (Monad m) => WS m a -> m (a, [HsDecl])
runWS = flip evalStateT 0 . runWriterT 

-- |Gets a new variable for us
wsNewVar :: (Monad m) => String -> WS m String
wsNewVar s = lift get >>= return . (split (+1) mkName)
                    >>= \(c', n) -> put c' >> return n
  where
    unnilpw ((), a) = a  
    
    mkName i = '_':s ++ show i
    
-- |Selects the needed variables, assign it to a new auxiliar decl,
--  adds a comment to the decl, with the variable names instead of their types
--  and returns the auxiliar function name.
wsSelect :: (Monad m, IsVar id) => [id] -> WS (OrdVSOME id m) HsExp
wsSelect sl
  = do
    s1 <- l2 ordvsGetVarSetPretty
    a  <- l2 $ ordvsAssoc sl
    s2 <- l2 ordvsGetVarSetPretty
    n  <- wsNewVar "a"
    tell $ [HsDeclComment (hsdeclSimpl n a) (s1 ++ "\n  -> " ++ s2)]
    return (HsEVar n) 
    
-- |Selects the needed variables, assign it to a new auxiliar decl,
--  adds a comment to the decl, with the variable names instead of their types
--  and returns the auxiliar function name.
wsCollect :: (Monad m, IsVar id) => [id] -> WS (OrdVSOME id m) HsExp
wsCollect sl
  = do
    s1 <- l2 ordvsGetVarSetPretty
    a  <- l2 $ ordvsCollect sl
    s2 <- l2 ordvsGetVarSetPretty
    n  <- wsNewVar "c"
    tell $ [HsDeclComment (hsdeclSimpl n a) (s1 ++ "\n  -> " ++ s2)]
    return (HsEVar n) 
    
-- |Wraps the maybe interface into our error monad.
wsGetv :: (IsVar id, MyMonad m) => id -> WS (OrdVSOME id m) HsExp
wsGetv v = l2 (ordvsGetV v >>= maybe (l2 $ errNoSuchVar1 v) return) 

-- |Translates a method to a HsDecl       
methodToMMM :: (IsVar id, MyMonad m) => MethodDef id -> OME m [HsStmt]
methodToMMM m@(Method n ps ret body)
  = let
    -- unwraps parameter names and types
    (vs, ts) = unzip ps
    -- fix the types (functions with void input type correspond to
    -- ()-typed machines) and builds a line pragma.
    (lp, ts', vs')= if vs == []
               then ([], [Simple TyVoid], [varBuild "NIL"])
               else ([HsSPragma $ uncurry HsPLine $ varLoc $ head vs], ts, vs) 
    in (ite <$> omIsConstructor m 
           <*> constructorToMMM vs' ts' m 
           <*> normalMethodToMMM vs' ts' m
       ) >>= return . (lp ++) . (:[])
  where
    -- translates a constructor to a mmm
    constructorToMMM vs ts (Method n _ _ body)
      = do
        mty  <- flip buildConsType (map hstyFromType $ ts) <$> omObjType
        (mbdy, a) <- runOrdVS . runWS $ (l2 (mapM ordvsNewV vs) >> trBody ts body)
        let name = map toLower n
        return $ HsSDecl $ HsDecl name (Just mty)
                 (HsEKlei (HsEComp HsEEta (HsEProj 2 1))
                 $ HsEKlei (HsEMMM mbdy) (HsEComp HsEEta (HsESplit (HsEVar "(const def)") HsEId)))
                 a
    
    -- |Returns our constructor's type.
    buildConsType :: [HsTy] -> [HsTy] -> HsTy
    buildConsType stty ps = hstyCtxStrongVar "m"
                          $ HsTyFun (HsTyProd ps) 
                          $ HsTyApp (HsTy1 $ HsTyVar "m") 
                                    (HsTyProd stty)
    
    -- translates a normal method to a mmm.
    normalMethodToMMM vs ts (Method n _ ret body)
      = do
        mty  <- flip buildMethodType (map hstyFromType $ ret:ts) <$> omObjType
        (mbdy, a) <- runOrdVS . runWS $ (l2 (mapM ordvsNewV vs) >> trBody ts body)
        name <- omQualifyName n
        return $ HsSDecl $ HsDecl name (Just mty) (HsEMMM mbdy) a
  
    -- builds a normal method type (a normal mmm)
    buildMethodType :: [HsTy] -> [HsTy] -> HsTy
    buildMethodType stty (r:ps) = hstyCtxStrongVar "m" 
                                $ HsTy1 $ HsTyMMM 
                                             (HsTy1 $ HsTyVar "m")
                                             (HsTyProd stty)
                                             (HsTyProd ps)
                                             r
    
    -- Translates a list of statements
    trBody :: (IsVar id, MyMonad m) => [Type id] -> StmtSeq id -> WS (OrdVSOME id m) HsMMM
    trBody ts body 
      = do
        -- first add a "button" to our varset (if we don't have one yet)
        snil   <- if (Simple TyVoid) `elem` ts
                    then return $ HsMLift HsEId
                    else HsMLift <$> (l2 $ ordvsAssoc []) 
        stmts' <- mapM trStmtTr body
        return (kleislify (snil:stmts'))

    kleislify []    = HsMCopy
    kleislify [h]   = h
    kleislify (h:t) = HsMKlei (kleislify t) h
        
    -- Translates a list of statements, internal use only.
    trBodyIf :: (IsVar id, MyMonad m) => StmtSeq id -> WS (OrdVSOME id m) HsMMM
    trBodyIf body = kleislify <$> mapM trStmtTr body
    
    -- "ordvsRwFor v f" will check if variable "v" is in scope. If it's not in scope,
    -- will declare it and run "f". If it's already in scope, will take care properly
    -- rewritting it.
    wsRwFor :: (IsVar id, MyMonad m) 
                       => id -> OrdVSOME id m HsMMM -> WS (OrdVSOME id m) HsMMM
    wsRwFor v f
      = l2 $ (ordvsGetV v >>=
        maybe (ordvsNewV v >> f)
              (\_ -> do
                let v' = varAppName "'" v
                ordvsRename v v'
                ordvsNewV v
                fres <- f
                ordvsRw v' v >>= maybe (l2 $ errNoSuchVar1 v)
                                       (\rw -> return (HsMKlei (HsMLift rw) fres))
              ))

    -- synonym for Select, Machine, Rearrange (or, deselect as is in the report)
    mkSM s m = HsMKlei m (HsMLift s)  
    
    trStmtTr :: (IsVar id, MyMonad m) => Stmt id -> WS (OrdVSOME id m) HsMMM
#ifdef __DEBUG__
    trStmtTr st
      = do
        T.trace (replicate 30 '>') (return ())
        T.trace ("> ST: " ++ show (pretty $ fmap varName st)) (return ())
        trStmt st
#else
    {-# INLINE trStmtTr #-}
    trStmtTr = trStmt
#endif
    
    -- translates statements.
    trStmt :: (IsVar id, MyMonad m) => Stmt id -> WS (OrdVSOME id m) HsMMM
    trStmt (StmtGet v a)
      = do
        s <- wsSelect []
        m <- wsRwFor a $ ((HsMVar <$> (lift $ omGetSt $ varName v)) >>= ordvsRunm)
        -- r <- ordvsRmFocus -- removing the focus point is the same as deselecting variables.
        return $ mkSM s m
    trStmt (StmtSet v a)
      = do
        s <- wsSelect [a]
        setter <- l2 $ lift $ omSetSt $ varName v
        -- r <- ordvsRmFocus
        return $ mkSM s (HsMRunmI (HsMVar setter))
    trStmt (StmtAssign (LhsVar v) exp)
      = do
        s <- wsSelect (varsOfExp exp)
        expmmm <- trExp exp
        e <- wsRwFor v $ ordvsRunm expmmm
        -- r <- ordvsRmFocus 
        return $ mkSM s e        
    trStmt (StmtRet (ExpVar v)) 
      = wsSelect [v] >>= return . HsMLift . HsEComp (HsEProj 2 1)
    trStmt (StmtIf (ExpVar c) th el)
      = do
        vs     <- l2 $ ordvsGetVs
        pic    <- wsGetv c
        vset   <- l2 $ ordvsGetVarSet
        thmmm  <- kleislify <$> mapM trStmtTr th
        vsetth <- l2 $ ordvsGetVarSet
        l2 $ ordvsPutVarSet vset
        elmmm  <- kleislify <$> mapM trStmtTr el
        unify  <- l2 $ ordvsUnifyWith vsetth
        return $ HsMCond pic thmmm (HsMKlei (HsMLift unify) elmmm)
    trStmt (StmtCall f ps)
      = do
        (HsMKlei m s) <- trExp (ExpCall f ps)
        m' <- l2 $ ordvsRunm_ m
        return $ HsMKlei m' s
    trStmt (StmtCollect v)
      = wsCollect v >>= return . HsMLift
    trStmt (StmtBlock [])
      = bug () "Empty-block statements are impossible. Send enough info to reproduce."
    trStmt (StmtBlock sts)
      = kleislify <$> mapM trStmtTr sts
    trStmt x
      = bug x "This statement should not appear here."
        
    
    -- translates expressions   
    trExp :: (IsVar id, MyMonad m) => Exp id -> WS (OrdVSOME id m) HsMMM
    trExp (ExpCall f ps) 
      = do
        let ps' = map (\ (ExpVar v) -> v) ps
        let l   = varAccessList f
        fexp <- l2 $ lift $ do
                fname <- case varAccessList f of 
                            ["this", obj, m] -> omQualifyMethodName obj m
                            _                -> bug (f, ps) "trExp: Not yet suported."
                ext   <- omExtSt $ head (tail l)   
                return $ ext $ HsMVar fname
        s <- wsSelect ps'
        return $ HsMKlei fexp (HsMLift s)
    trExp exp
      = let
        expvs = map varName $ varsOfExp exp
      in return $ HsMLift $ HsELam expvs (HsEOOP $ fmap varName exp)

--------------------------------------------------------------------------------
-- * Local definitions

intersperse' :: [a] -> [a] -> [a]
intersperse' [] _ = []
intersperse' _ [] = []
intersperse' (h1:t1) (h2:t2) = h1:h2:(intersperse' t1 t2)        
