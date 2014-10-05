{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ObjSt
-- Copyright   :  HASLAb team, University of Minho
-- License     :  MIT
--
-- Maintainer  :  Victor Miraldo <victor.cacciari@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
--Takes care of the state-monadic part of the object, translates
--the generalized state extension to a binary one. Also provides
--naming utilities such as qualification and scope-checking.
----------------------------------------------------------------------------
module MMM.OOP.ObjSt(
  -- * Types
    OM
  -- * Functionality
  , runObjSt
  , omIsConstructor
  , omPiX
  , omGetSt
  , omSetSt
  , omCard
  , omIdxOf
  , omObjType
  , omObjName
  , omTypeOf
  , omExtSt
  , omGetStType
  , omSetStType
  , omQualifyName
  , omQualifyMethodName
  ) where

import MMM.Core.All hiding (L)
import MMM.HsMMM
import MMM.OOP.Language.Syntax
import MMM.Util.Util

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State

import Data.Char(toUpper, toLower)
  
--------------------------------------------------------------------------------
-- * Instance
  
-- |the order is important.
type OM = StateT (String, [(String, HsTy)])

-- |Unwrapping of a OM, given a class.
runObjSt :: (IsVar id, Monad m) => Class id -> OM m a -> m a
runObjSt c
  = let
    name = className c
    stvs = map (\(VarDef i t) -> (varName i, t)) $ classDefVars $ classDef c
  in flip evalStateT (name, map (id >< hstyFromType) stvs)
  
-- |Returns true if the given method corresponds to the current object's constructor.
omIsConstructor :: (Monad m, IsVar id) => MethodDef id -> OM m Bool
omIsConstructor (Method n _ _ _) = omObjName >>= return . (== n)
  
-- |Returns the current object name
omObjName :: (Monad m) => OM m String
omObjName = get >>= return . p1

-- |Qualifies a name for the current object.
omQualifyName :: (Monad m) => String -> OM m String
omQualifyName s = omObjName >>= return . (++ (map toUpper s)) . map toLower

-- |Returns the name for a given method
omQualifyMethodName :: (Monad m) => String -> String -> OM m String
omQualifyMethodName var m
  = do
    ty <- omTypeOf var
    case ty of
      (HsTy1 (HsTyVar s)) -> return $ map toLower s ++ map toUpper m
      _                   -> bug (var, ty, m) "This variable has no methods."

-- |Retursn the current object type.
omObjType :: (Monad m) => OM m [HsTy]
omObjType = get >>= return . p2 . unzip . p2

-- |Returns the type of a variable
omTypeOf :: (Monad m) => String -> OM m HsTy
omTypeOf id = get >>= return . snd . shead . filter ((== id) . fst) . snd
  where
    shead [] = error $ "variable " ++ show id ++ " is not registered."
    shead l  = head l

-- |omCardinality of our current state.
omCard :: (Monad m) => OM m Int
omCard = get >>= return . length . snd

-- |Returns the address of a variable, -1 if its not declared.
omIdxOf :: (Monad m) => String -> OM m Int
omIdxOf x = get >>= return . search 0 x . snd
  where
    search i x ((x', _):t)
      | x == x'   = i
      | otherwise = search (i+1) x t
    search _ v l = bug (v, l) "critical. Reimplement ObjSt to something more stable!!!!"
  
-- |Generalized state extension
--  given an identifier @o@, and a machine @m@,
--  will return a machine that runs on the current object,
--  but leaves everything but @o@ in it's state unchanged. 
omExtSt :: (Monad m) => String -> OM m (HsMMM -> HsMMM)
omExtSt x
  = do
    c <- omCard
    i <- omIdxOf x
    return $ binaryExt c (i+1)
  where
    binaryExt :: Int -> Int -> HsMMM -> HsMMM
    binaryExt max i
      | i > max   = error "critical... binaryExt on ObjSt.hs"
      | otherwise = (HsMExtR ^^^ (max - i)) . (HsMExtL ^^^ (dirac i))

-- |this is the \pi_x in the report.
omPiX :: (Monad m) => String -> OM m HsExp
omPiX x
  = do
    c <- omCard
    i <- omIdxOf x
    if c == 1
      then return $ HsEId
      else return $ HsEProj c (i+1)
    -- return $ MSimple $ (MFComp (MFProj 1)) ^^^ (c - i) $ (MFComp (MFProj 2)) ^^^ (dirac i) $ MFIden

-- |Returns the default name for the getter of @v@ in the current object
omGetSt :: (Monad m) => String -> OM m String    
omGetSt v
      = do
        (cid, _) <- get
        return $ "get_" ++ varName v ++ "_" ++ cid

-- |Returns the getter type       
omGetStType :: (Monad m) => String -> OM m HsTy
omGetStType v
  = do
    vty <- omTypeOf v
    sty <- omObjType
    return $ hstyCtxStrongVar "m" $ HsTy1 
           $ HsTyMMM (HsTy1 $ HsTyVar "m")
                     (HsTyProd sty)
                     HsTyNil
                     vty

-- |Returns the default name for the getter of @v@ in the current object 
omSetSt :: (Monad m) => String -> OM m String   
omSetSt v
  = do
    (cid, _) <- get
    return $ "set_" ++ varName v ++ "_" ++ cid
    
-- |Returns the setter type       
omSetStType :: (Monad m) => String -> OM m HsTy
omSetStType v
  = do
    vty <- omTypeOf v
    sty <- omObjType
    return $ hstyCtxStrongVar "m" $ HsTy1 
           $ HsTyMMM (HsTy1 $ HsTyVar "m")
                     (HsTyProd sty)
                     vty
                     HsTyNil
    
