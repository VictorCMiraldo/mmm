{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Util
-- Copyright   :  HASLAb team, University of Minho
-- License     :  MIT
--
-- Maintainer  :  Victor Miraldo <victor.cacciari@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
--General coding utilities. Mostly syntax-sugar for the programmer, you shouldn't
--need to read this.
----------------------------------------------------------------------------
module MMM.Util.Util(
    module Control.Monad
  , module Control.Monad.Error
  , module Control.Monad.State
  , module Control.Monad.Writer
  , module Control.Applicative
  , MyMonad
  , E, errNoSuchVar1, errNoSuchVar
  , bug
  , author
  ) where

import Control.Monad
import Control.Monad.Error hiding (getAll)
import Control.Monad.State
import Control.Monad.Writer
import Control.Applicative

import MMM.OOP.Language.Syntax(IsVar(..))

-------------------------------------------------------------------------------
-- * Usefull Classes; abreviation for contexts only.

class (Monad m, Functor m, Applicative m) => MyMonad m where

instance MyMonad IO where

--------------------------------------------------------------------------------
-- * Error Handling

data Err
  = ErrLoc (Int, String) String
  | ErrNoLoc String

instance Show Err where
  show (ErrNoLoc s) = s
  show (ErrLoc (l, f) s) = f ++ ":" ++ show l ++ ":" ++ s
  
instance Error Err where
  strMsg = ErrNoLoc
  
type E = ErrorT Err

instance (MyMonad m) => MyMonad (E m) where

errNoSuchVar1 :: (MyMonad m, IsVar id) => id -> E m a
errNoSuchVar1 v = throwError $ ErrLoc (varLoc v) $ "Undefined variable: " ++ varName v

errNoSuchVar :: (MyMonad m, IsVar id) => [id] -> E m a
errNoSuchVar vs = throwError $ ErrLoc (varLoc $ head vs) $ "Undefined variable: " ++ concatMap ((++ ", ") . varName) vs

--------------------------------------------------------------------------------
-- * General Utilities

author :: String
author = "Victor C. Miraldo <victor.cacciari@gmail.com>"

bug :: (Show e) => e -> String -> a
bug e s = error $ "Smells like it's burning... Sorry, my bad. Please report this bug to " 
                ++ author ++ "\n[ERROR DESC]: "
                ++ s      ++ "\n[TECH INFO ]: "
                ++ show e ++ "\n\n"
