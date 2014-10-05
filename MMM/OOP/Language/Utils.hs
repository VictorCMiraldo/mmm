{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Utils
-- Copyright   :  HASLAb team, University of Minho
-- License     :  MIT
--
-- Maintainer  :  Victor Miraldo <victor.cacciari@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
--Provides general language related utilities, such as simplifications and
--code transformations.
----------------------------------------------------------------------------
module MMM.OOP.Language.Utils(
  -- * Types
    SM
  , SMWR
  -- * Functions
  , runSM
  , smCount
  , smwrWriteExps
  , unwr
  -- * Modules
  , module Data.Generics.Uniplate.Data
  -- * Simplifications
  , simplify
  , simplify1
  , simplifyAll
  , simplifyDebugAllStages
  , simplifyDebugOneStage
  , simplifyDebug
  , SimpStage(..)
  ) where

import MMM.Core.FuncComb as PF

import MMM.OOP.Language.Syntax

import MMM.Util.Pretty

import Data.Generics.Uniplate.Data
import Data.Typeable
import Data.Data

import Control.Applicative
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Writer
import qualified Data.Map as M
import Data.List(intersect, (\\))

import qualified Debug.Trace as T

-- TODO:
{-
    1. Apply simple substitutions:
          if we have, for instance: i = pure0;
          we can erase this statement and change every read
          of i by pure0, until we find a different assignment to i.

-}

-------------------------------------------------------------------------------
-- * Simplification Pipeline

-- ** Interface

data SimpStage
  = SimpPurify 
  | SimpExplin 
  | SimpParmlin 
  | SimpIfcomp 
  | SimpLiftgetset 
  | SimpVoidcomp
  | SimpNosimp
  deriving (Eq, Show)
  
instance Pretty SimpStage where
  pretty SimpPurify  = t "Purification"
  pretty SimpExplin  = t "Expression Lin."
  pretty SimpParmlin = t "Parameter Lin."
  pretty SimpIfcomp  = t "If Completion"
  pretty SimpLiftgetset = t "Lift Getters Setters"
  pretty SimpNosimp  = t "No simplification"
  pretty SimpVoidcomp = t "Void Completion"
  
allStages :: [SimpStage]
allStages = [ SimpNosimp, SimpParmlin, SimpExplin 
            , SimpPurify, SimpLiftgetset, SimpIfcomp
            , SimpVoidcomp
            ]
  
-- |Applies a chain of simplification procedures to a class def.
simplify :: (Monad m) => [SimpStage] -> ClassDef Var -> m (ClassDef Var)
simplify sl cdef = runSM $ foldl (\a h -> a >>= simplify1 h) (return cdef) sl

-- |Applies the full simplification pipeline
simplifyAll :: (Monad m) => ClassDef Var -> m (ClassDef Var)
simplifyAll = simplify allStages

-- |Applies one simplification routine to a ClassDef
simplify1 :: (Monad m) => SimpStage -> ClassDef Var -> SM m (ClassDef Var)
simplify1 SimpNosimp def = return def 
simplify1 SimpPurify def = purify def
simplify1 SimpExplin def = explin def 
simplify1 SimpParmlin def = parmlin def 
simplify1 SimpIfcomp def = ifcomplete def 
simplify1 SimpLiftgetset def = liftgetset def 
simplify1 SimpVoidcomp def = completevoids def

-- ** Debugging Versions
  
simplifyDebugAllStages :: ClassDef Var -> IO (ClassDef Var)
simplifyDebugAllStages = simplifyDebug allStages

simplifyDebugOneStage :: SimpStage -> ClassDef Var -> SM IO (ClassDef Var)
simplifyDebugOneStage s def = simplify1 s def >>= dump (show . pretty $ s)

simplifyDebug :: [SimpStage] -> ClassDef Var -> IO (ClassDef Var)
simplifyDebug sl cdef = runSM $ foldl (\a h -> a >>= simplifyDebugOneStage h) (return cdef) sl

mPutStrLn = lift . putStrLn
    
dump lbl d
  = do
    mPutStrLn (replicate 40 '-')
    mPutStrLn ("STAGE:  " ++ lbl ++ "\n")
    dumpCode d
    mPutStrLn (replicate 40 '-')
    return d
  where dumpCode = mPutStrLn . show . pretty
      

--------------------------------------------------------------------------------
-- * Simplification Transformer

-- |Provides a counter and error-handling.
type SM m = StateT Int m

-- |Provides easy "code insertion" through a MonadWriter.
type SMWR id m = WriterT (StmtSeq id) (SM m)

-- |Unwrap the SM into a monad.
runSM :: (Monad m) => SM m a -> m a
runSM = flip evalStateT 0

-- |Increases the counter.
smCount :: (Monad m) => SM m Int
smCount = get >>= \r -> modify (+1) >> return r

-- |Takes the writer out of the SM
unwr :: (Monad m) => SMWR id m a -> SM m (a, StmtSeq id)
unwr = runWriterT

-- |Applies a function @f@ to all Expressions in a given StmtSeq, provided with a @id@ converssion @k@.
smwrWriteExps :: (Monad m)
              => (Exp id -> SMWR id' m (Exp id'))
              -> (id -> id')
              -> StmtSeq id
              -> SM m (StmtSeq id')
smwrWriteExps f k l = mapM simpl l >>= return . concat
  where
    simpl (StmtAssign lhs exp) = unwr (f exp)     >>= rebuild (StmtAssign (fmap k lhs)) 
    simpl (StmtCall   id ps)   = unwr (mapM f ps) >>= rebuild (StmtCall (k id))
    simpl (StmtRet    es)      = unwr (f es) >>= rebuild StmtRet
    simpl (StmtIf c t e)
      = do 
        t' <- smwrWriteExps f k t
        e' <- smwrWriteExps f k e
        unwr (f c) >>= rebuild (\c' -> StmtIf c' t' e') 
    
    rebuild :: (Monad m) => (a -> Stmt id') -> (a, StmtSeq id') -> m (StmtSeq id')
    rebuild ctr = return . (uncurry (++)) . swap . (((:[]) . ctr) >< id)

--------------------------------------------------------------------------------
-- * Code Simplifications

-- |Boilerplate for applying a function to every method in a ClassDef
bpOnMethod :: (Monad m) => (MethodDef id -> m (MethodDef id)) 
                        -> ClassDef id -> m (ClassDef id)
bpOnMethod f (ClassDef vs ms) = mapM f ms >>= return . ClassDef vs

-- |Boilerplate for applying a function to a method's body.
bpOnMethodBody :: (Monad m) => (StmtSeq id -> m (StmtSeq id)) 
                            -> MethodDef id -> m (MethodDef id)
bpOnMethodBody f m = f (methodBody m) >>= \b -> return $ m { methodBody = b }

-- |Simple way to retrieve a new variable indexed by a string.
newVar :: (Monad m) => String -> SM m Var
newVar s = smCount >>= return . SimpleVar voidLoc . (s ++) . show

mkAssign var exp = StmtAssign (LhsVar var) exp

-- |Purify expressions.
--  Isolate every inpure statement (state-changing) from
--  the rest.
--
--  purify "x = obj.func(y + 1) * 5"
--  ==
--  aux = obj.func(y+1);
--  x = aux*5;
--
purify :: (Monad m) => ClassDef Var -> SM m (ClassDef Var)
purify = bpOnMethod (bpOnMethodBody (smwrWriteExps (transformBiM conv) id))
  where
    nv :: (Monad m) => SMWR Var m Var
    nv = lift $ newVar "_pure"
    
    conv :: (Monad m) => Exp Var -> SMWR Var m (Exp Var)
    conv (ExpVar v)
      | varIsPure v = return (ExpVar v)
      | otherwise   
      = do 
        aux <- nv
        tell $ [mkAssign aux (ExpVar v)]
        return (ExpVar aux)                   
    conv (ExpCall f ps)
      = do
        nvs <- mapM (const nv) ps -- one new variable for each param.
        res <- nv                 -- and a new one for the result.
        let stmts = map (uncurry mkAssign) $ zip nvs ps
        tell $ stmts ++ [mkAssign res $ ExpCall f (map ExpVar nvs)]
        return (ExpVar res)       
    conv x = return x
    
-- |Parameter Linearization
--  Adds auxiliar variables instead of expressions in function parameters.
parmlin :: (Monad m) => ClassDef Var -> SM m (ClassDef Var)
parmlin = bpOnMethod (bpOnMethodBody rwStmtWriter)
  where
    rwStmtWriter sts = runWriterT (mapM rwStmt sts) >>= return . uncurry (++) . swap
        
    rwStmt (StmtCall f ps)
      = mapM mkAssignNewVar ps >>= return . StmtCall f
    rwStmt (StmtIf c th el)
      = do
        c'  <- mkAssignNewVar c
        th' <- lift $ rwStmtWriter th
        el' <- lift $ rwStmtWriter el
        return $ StmtIf c' th' el'
    rwStmt x@(StmtRet (ExpVar v))
      = return x
    rwStmt x@(StmtRet e)
      = do
        e' <- mkAssignNewVar e
        return $ StmtRet e' 
    rwStmt x 
      = return x
        
    mkAssignNewVar e
      | expIsLinear e && not (expIsLit e) = return e
      | otherwise
        = do
          nv <- lift $ newVar "_parmlin"
          tell $ [mkAssign nv e]
          return (ExpVar nv)
          
    

-- |Expression Linearization
--  A linear expression is one that has, at most, one operator.
explin :: (Monad m) => ClassDef Var -> SM m (ClassDef Var)
explin = bpOnMethod (bpOnMethodBody (smwrWriteExps explinaux id))
  where        
    explinaux :: (Monad m) => Exp Var -> WriterT [Stmt Var] (SM m) (Exp Var)
    explinaux e@(ExpBinop op l r)
      | expIsLinear l && expIsLinear r = return e
      | expIsLinear l = explinAssign r >>= return . (ExpBinop op l)
      | expIsLinear r = explinAssign l >>= return . (\l -> ExpBinop op l r)
      | otherwise    
        = do
          vl <- explinAssign l
          vr <- explinAssign r
          return $ ExpBinop op vl vr
    explinaux x = return x
     
    explinAssign :: (Monad m) => Exp Var -> WriterT [Stmt Var] (SM m) (Exp Var)     
    explinAssign e 
      = do
        nv <- lift $ newVar "_explin"
        e' <- explinaux e
        tell $ [mkAssign nv e']
        return (ExpVar nv)    
        
-- |Complete the branches of a if-statements.
ifcomplete :: (Monad m) => ClassDef Var -> SM m (ClassDef Var)
ifcomplete = bpOnMethod (bpOnMethodBody (\s -> mapM rwIfs s >>= return . stmtRmvBlocks))
  where
    getAssignedVars :: StmtSeq Var -> [Var]
    getAssignedVars s = [ v | (StmtAssign (LhsVar v) _) <- universeBi s ]
                     ++ [ v | (StmtGet _ v) <- universeBi s ]

    rwIfs :: (Monad m) => Stmt Var -> m (Stmt Var)
    rwIfs (StmtIf (ExpVar c) th el)
      = do
        th' <- mapM rwIfs th
        el' <- mapM rwIfs el
        let vth = getAssignedVars th'
        let vel = getAssignedVars el'
        let vthel = vth `intersect` vel
        let vthonly = vth \\ vthel
        let velonly = vel \\ vthel
        return $ StmtIf (ExpVar c) (complete th vthonly) (complete el velonly)        
    rwIfs (StmtIf _ _ _)
      = error "OOP.Language.Utils: ifdrop: if-statement conditions must be single variables at this point."
    rwIfs x = return x
    
    complete sts [] = sts
    complete sts vs = sts ++ [StmtCollect vs]
    
-- |Complete Methods
completevoids :: (Monad m) => ClassDef Var -> SM m (ClassDef Var)
completevoids = bpOnMethod rwIfVoid
  where
    rwIfVoid :: (Monad m) => MethodDef Var -> SM m (MethodDef Var)
    rwIfVoid m@(Method _ _ ret b)
      | ret == Simple TyVoid = return m{ methodBody = b ++ [StmtRet $ ExpVar (varBuild "NIL")] }
      | otherwise            = return m
  
-- |Lift getting and setting state variables from expressions to statements.
liftgetset :: (Monad m) => ClassDef Var -> SM m (ClassDef Var)
liftgetset =  bpOnMethod (bpOnMethodBody ( (>>= return . stmtRmvBlocks) . rwStmtWriter))
  where
    rwStmtWriter sts = runWriterT (mapM trGetSet sts) >>= return . uncurry (++) 
    
    trGetSet (StmtAssign (LhsVar v) e)
      | varIsPure v = possiblyGet v e
      | otherwise   = possiblySet v e >>= return . StmtBlock
    trGetSet (StmtIf c th el)
      = do
        th' <- lift $ rwStmtWriter th
        el' <- lift $ rwStmtWriter el
        return $ StmtIf c th' el'
    trGetSet x = return x
    
    possiblySet v (ExpVar ev)
      = return $ [StmtSet v ev]
    possiblySet v e
      = do
        nv <- lift $ newVar "_liftset"
        return $ [mkAssign nv e, StmtSet v nv]
        
    isGetter (ExpVar v) = not $ varIsPure v
    isGetter _          = False
        
    possiblyGet v ev@(ExpVar ev')
      | isGetter ev = return $ StmtGet ev' v
      | otherwise   = return $ StmtAssign (LhsVar v) ev
    possiblyGet v ev@(ExpBinop o l r)
      | isGetter l
        = do
          nv <- lift $ newVar "_liftget"
          let (ExpVar l') = l
          tell $ [StmtGet l' nv]
          return $ mkAssign v (ExpBinop o (ExpVar nv) r)
      | isGetter r
        = do
          nv <- lift $ newVar "_liftget"
          let (ExpVar r') = r
          tell $ [StmtGet r' nv]
          return $ mkAssign v (ExpBinop o l (ExpVar nv))
      | otherwise = return $ mkAssign v ev
    possiblyGet v ev = return $ mkAssign v ev
