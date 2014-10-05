{-# LANGUAGE DeriveDataTypeable   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  SSA
-- Copyright   :  HASLAb team, University of Minho
-- License     :  MIT
--
-- Maintainer  :  Victor Miraldo <victor.cacciari@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
--NOT USED ANYMORE
--This module is here just for completness sake. We don't use SSA translation
--anymore but it could be helpfull later on.
----------------------------------------------------------------------------
module MMM.OOP.SSA where

import MMM.OOP.Language.Syntax
import MMM.OOP.Language.Utils

import MMM.Util.Pretty

import Data.Typeable
import Data.Data

import qualified Data.Map as M
import Data.Maybe
import Data.List(intersect, (\\))

import Control.Applicative
import Control.Monad.State
import Control.Monad.Identity

--------------------------------------------------------------------------------
-- * SSA Identifiers

data SSAVar
  = Versioned Var Int
  | Unversioned Var
  deriving (Eq, Show, Data, Typeable)
  
instance Pretty SSAVar where
  pretty (Versioned v i) = pretty v <> t "#" <> pretty i
  pretty (Unversioned v) = pretty v <> t "!#"
  
instance Ord SSAVar where
  compare (Versioned v i) (Versioned v' i')
    | v == v'   = compare i i'
    | otherwise = compare v v'
  compare (Unversioned v) (Unversioned v') = compare v v'
  compare _ _ = error "I believe this will never happen, more tests are needed though."
  
--------------------------------------------------------------------------------
-- * SSA Translation

data SSAst = SSAst
  { wdict :: M.Map String Int
  , rdict :: M.Map String Int
  } deriving (Show)

type SSAM m = StateT SSAst m

--------------------------------------------------------------------------------
-- ** Utilities

showV :: (Pretty a) => a -> String
showV = show . pretty

incRd :: (Monad m) => Var -> SSAM m ()
incRd n = modify (\s -> s { rdict = M.adjust (+1) (showV n) (rdict s)})

incWr :: (Monad m) => Var -> SSAM m ()
incWr n = modify (\s -> s { wdict = M.adjust (+1) (showV n) (wdict s)})

getRd :: (Monad m) => Var -> SSAM m (Maybe Int)
getRd n = get >>= return . (M.lookup (showV n)) . rdict

addVar :: (Monad m) => Var -> SSAM m ()
addVar n = modify (\s -> s { rdict = M.insert (showV n) (0-1) (rdict s)
                           , wdict = M.insert (showV n) 0 (wdict s)
                           })

getWr :: (Monad m) => Var -> SSAM m Int
getWr n = get >>= \sts ->
  case M.lookup (showV n) (wdict sts) of
    Nothing -> addVar n >> return 0
    Just v  -> return v
    
--------------------------------------------------------------------------------
-- ** Translation

ssaMethod :: (Monad m, Functor m) => MethodDef Var -> m (MethodDef SSAVar)
ssaMethod m@(Method _ ps _ _)
  = evalStateT (toSSA m) (init ps)
  where
    init ps = let names = map (showV . fst) ps
                  aux   = M.fromList $ zip names $ repeat 0
              in SSAst (M.map (+1) aux) aux
              
    toSSA (Method n ps ret sts)
      = Method n <$> mapM ssaInstParms ps <*> return (fmap Unversioned ret) <*> ssaStmtSeq sts
      
    ssaInstParms (n, ty) = getRd n >>= \m -> return (Versioned n (fromJust m), fmap Unversioned ty)

ssaStmtSeq :: (Monad m, Functor m) => StmtSeq Var -> SSAM m (StmtSeq SSAVar)
ssaStmtSeq l = concat <$> mapM ssaStmt l
  where
    ssaStmt (StmtAssign lhs exp) = sgl <$> (StmtAssign <$> ssaLhs lhs <*> (ssaExp exp <* incRd (lhsVar lhs)))
    ssaStmt (StmtRet exp)        = sgl <$> (StmtRet <$> ssaExp exp)
    ssaStmt (StmtCall f ps)      = sgl <$> (StmtCall (Unversioned f) <$> mapM ssaExp ps)
    ssaStmt (StmtIf c t e)
      = do
        let var = (SimpleVar voidLoc "__cond")
        vC  <- Versioned var <$> (getWr var <* incWr var)
        c'  <- ssaExp c
        dbt <- get
        t'  <- ssaStmtSeq t
        dbe <- get
        e'  <- ssaStmtSeq e
        dae <- get
        phi <- buildPhis vC dbt dbe dae
        return $ (StmtAssign (LhsVar vC) c') 
               : (StmtIf (ExpVar vC) t' e')
               : phi
        
    sgl x = [x]  
       
    buildPhis c bt be ae
      = let
        dT  = getDiffs (wdict bt) (wdict be)
        dE  = getDiffs (wdict be) (wdict ae)
        dTE = dT `intersect` dE
        dT'   = dT \\ dTE
        dE'   = dE \\ dTE
      in do
        condsT  <- mapM (makePhi c (rdict be) (rdict bt)) dT'
        condsE  <- mapM (makePhi c (rdict bt) (rdict ae)) dE'
        condsTE <- mapM (makePhi c (rdict be) (rdict ae)) dTE
        return $ map fromJust $ filter isJust $ condsT ++ condsE ++ condsTE
        
    -- Return the different keys in two Map String Int
    getDiffs m2 m1
      = let
        f i1 i2 = if i1 /= i2 then Just i1 else Nothing
      in filter ((/= "__") . take 2) $ M.keys $ M.differenceWith f m1 m2
        
    -- makes a "phi" statement with condition var c, dictionary of branch where
    -- c holds "yesC", analogous "noC", for assigning variable "var". 
    makePhi c yesC noC var 
      = do
        let n = SimpleVar voidLoc var
        let yesV = M.lookup var yesC
        let noV  = M.lookup var noC
        if isJust yesV && isJust noV
          then do
            let yesCv = Versioned n $ fromJust yesV
            let noCv  = Versioned n $ fromJust noV
            v <- getWr n <* incWr n <* incRd n
            return $ Just (StmtPhi (Versioned n v) c yesCv noCv)
          else
            return Nothing  
      
    
    ssaLhs :: (Monad m, Functor m) => Lhs Var -> SSAM m (Lhs SSAVar)
    ssaLhs (LhsVar id@(SimpleVar _ _)) = LhsVar . (Versioned id) <$> (getWr id <* incWr id)
    ssaLhs (LhsVar id) = return $ LhsVar (Unversioned id)
 
    lhsVar :: Lhs Var -> Var
    lhsVar (LhsVar id) = id
    
    ssaExp :: (Monad m, Functor m) => Exp Var -> SSAM m (Exp SSAVar)
    ssaExp = instRd
      where
        instRd (ExpVar i)    = ExpVar . maybe (Unversioned i) (Versioned i) <$> (getRd i)
        instRd (ExpCall f e) = ExpCall (Unversioned f) <$> mapM ssaExp e
        instRd (ExpUnop o e) = ExpUnop o <$> ssaExp e
        instRd (ExpBinop o m n) = ExpBinop o <$> ssaExp m <*> ssaExp n
        instRd (ExpLit l)    = return (ExpLit l)

        
