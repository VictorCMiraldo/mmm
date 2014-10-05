{-# LANGUAGE FlexibleContexts       #-} 
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-} 
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distributive
-- Copyright   :  HASLAb team, University of Minho
-- License     :  MIT
--
-- Maintainer  :  Victor Miraldo <victor.cacciari@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
--This module provides a distributive law for Monads. It's a nice alternative
--to transformers (when you dont have them available).
----------------------------------------------------------------------------
module MMM.Core.Distributive (
      Distributive(..)
    , Lift(..)
    , lft
    , unlft
    ) where
    
import MMM.Core.Probability
import MMM.Qais

import Control.Monad
import Control.Monad.Free as F
import Data.Maybe

import MMM.Core.FuncComb

--------------------------------------------------------------------------------
-- * Distributive Law

-- |The distributive law on monads is a natural transformation,
--  providing, for each object, a function of type @ft -> tf@
class (Functor f, Functor t) => Distributive t f where
  lambda :: f (t a) -> t (f a)
  
--------------------------------------------------------------------------------
-- ** Some Instances

-- If we have a monad @f@, @Maybe@ is trivially distributed through @f@  
instance (Functor f, Monad f) => Distributive f Maybe where
  lambda = maybe (return Nothing) (fmap Just)

-- Any monad distributes over lists, 'Control.Monad.sequence' is the
-- distributive law
instance (Functor f, Monad f) => Distributive f [] where
  lambda = sequence
  
-- As we saw in the JLAMP14 paper, the free monads are also distributive.
instance (Functor f, Functor g, Distributive f g)
         => Distributive f (Free g) where
  lambda = cataF (either (fmap Pure) (fmap Free . lambda))
  
-- We need some auxiliar definitions over the Free Monad
outF :: (Functor g) => Free g a -> Either a (g (Free g a))
outF (Pure a) = Left a
outF (Free f) = Right f
  
cataF :: (Functor g) => (Either a (g b) -> b) -> Free g a -> b
cataF gene = gene . (id -|- fmap (cataF gene)) . outF
  
--------------------------------------------------------------------------------
-- * Distributive Law

--  The only reason for using this constructor is
--  the hability to use standard monads, instead of
--  transformers, in our behaviour stack.
--  The best example is the Dist monad, we have no transformer
--  version of such monad yet, in the paper, we handle machines
--  with behaviour Dist . MaybeT; This constructor allows
--  us to handle them in Haskell too.

-- |We know that the lifting of a functor (monad) by a monad is equivalent to
--  the existence of a distributive law. The Lift object acts as a Monad Transformer
--  for monads with a distributive law.
data Lift :: (* -> *) -> (* -> *) -> * -> *
  where
    L :: (Monad t, Functor f) => t (f a) -> Lift t f a
    
unlft :: (Monad t, Distributive t f) => Lift t f a -> t (f a)
unlft (L f) = f
    
-- |We have to provide a corresponding lift function, like the MonadTrans class.
lft :: (Monad f, Monad t, Distributive t f) => f a -> Lift t f a
lft = L . return

-- |In fact, if we have two monads @t@ and @f@, such that @t@ and @f@ distribute,
--  then @tf@ is also a monad.
instance (Monad f, Monad t, Distributive t f)
       => Monad (Lift t f) where
  return      = L . return . return

  (L x) >>= k = L $ x >>= fmap join . lambda . (>>= return . unlft . k)

-- |Lifting of functors is, obviously, a functor.      
instance (Distributive t f, Monad t) => Functor (Lift t f) where
  fmap f a = L $ fmap (fmap f) (unlft a)

-- |Lifting of Strong Monads remains a Strong Monad (in the base cateogry, Hask in 
--  our case).
instance (Distributive t f, Strong f, Strong t) => Strong (Lift t f) where
  rstr (x, b) = do a <- x; return (a, b)
  lstr (b, x) = do a <- x; return (b, a)
  
--------------------------------------------------------------------------------
-- ** Routine Instances

-- |We might want to show lifted values.
instance (Show (t (f a))) => Show (Lift t f a) where
  show (L f) = show f
  
-- |Or compare lifted values.
instance (Eq (t (f a))) => Eq (Lift t f a) where
  (L x) == (L y) = x == y
  
--------------------------------------------------------------------------------
-- ** Testing

-- |Constant functor
data K a = K
  deriving (Show, Ord, Eq)
  
instance Functor K where
  fmap _ = const K
  
instance (Functor f, Monad f) => Distributive f K where
  lambda _ = return K
  
-- |Maybe as a instance of the free monad for K
type FreeMaybe = Free K

nothing :: FreeMaybe a
nothing = Free K

-- |Totalization of sqrt using the FreeMaybe
sqrtTotal :: Float -> FreeMaybe Float
sqrtTotal n
  | n < 0      = nothing
  | otherwise  = return $ sqrt n
  
-- |Lifting of sqrtTotal
sqrtFaulty :: Float -> Lift Dist FreeMaybe Float
sqrtFaulty = lft . sqrtTotal
  
-- |Faulty addition. Works 90% of the time, return m on the other 10%.
additionFaulty :: Float -> Float -> Lift Dist FreeMaybe Float
additionFaulty m = L . schoice 0.9 (add m) (err m)
  where
    add m n = return $ m + n
    err m _ = return $ m
    
-- |Infix version
(.+) :: Float -> Float -> Lift Dist FreeMaybe Float
(.+) m n = additionFaulty m n 

-- |Quadratic Solver
bhaskara :: Float -> Float -> Float -> Lift Dist FreeMaybe (Float, Float)
bhaskara a b c
  = do
    let d1 = b^2
    let d2 = -4*a*c
    dsqr <- (d1 .+ d2) >>= sqrtFaulty
    let b' = -b
    x1    <- b' .+ dsqr
    x2    <- b' .+ (-dsqr)
    return (x1 / (2*a), x2 / (2*a))

-- |Quadratic equation function
quadratic :: Float -> Float -> Float -> Float -> Float
quadratic a b c x = a * x^2 + b * x + c
    
