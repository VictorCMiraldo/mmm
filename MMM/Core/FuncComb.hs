{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
-- (c) MP-I, CP, IPM (1998/99-2011/12)

-- |The most used pointfree combinators are defined in this module.
module MMM.Core.FuncComb where

import Control.Monad.Identity(Identity)

infix 5  ><
infix 4  -|-

-- Function exponentiation
(^^^) :: (a -> a) -> Int -> a -> a
f ^^^ 0 = id
f ^^^ n = f . (f ^^^ (n-1))

-- A dirac on integers.
dirac :: Int -> Int
dirac 1 = 0
dirac _ = 1

-- (1) Product -----------------------------------------------------------------

split :: (a -> b) -> (a -> c) -> a -> (b,c)
split f g x = (f x, g x)

(><) :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
f >< g = split (f . p1) (g . p2)

-- the 0-adic split 

(!), bang :: a -> ()
(!) = const ()
bang = (!)

-- Renamings:

p1 :: (a, b) -> a
p1 = fst

p2 :: (a, b) -> b
p2 = snd

-- (2) Coproduct ---------------------------------------------------------------

-- Renamings:

i1 :: a -> Either a b
i1 = Left

i2 :: b -> Either a b 
i2 = Right

i11 :: a -> Either (Either a b) (Either c d)
i11 = i1 . i1

i12 :: b -> Either (Either a b) (Either c d) 
i12 = i1 . i2

i21 :: c -> Either (Either a b) (Either c d) 
i21 = i2 . i1

i22 :: d -> Either (Either a b) (Either c d)
i22 = i2 . i2

-- either is predefined

(-|-) :: (a -> b) -> (c -> d) -> Either a c -> Either b d
f -|- g = either (i1 . f) (i2 . g)

-- McCarthy's conditional:

mccarthy :: (b -> Bool) -> (b -> c) -> (b -> c) -> b -> c
mccarthy p f g = (either f g) . (grd p)

-- If-then-else as a function:

ite :: Bool -> x -> x -> x
ite i t e = if i then t else e

-- (3) Exponentiation ---------------------------------------------------------

-- curry is predefined

ap :: (a -> b,a) -> b
ap = uncurry ($)

expn :: (b -> c) -> (a -> b) -> a -> c
expn f = curry (f . ap)

p2p :: (b, b) -> Bool -> b
p2p p b = if b then (snd p) else (fst p) -- pair to predicate

-- exponentiation functor is (a->) predefined 
{-
instance Functor ((->) s) where
	fmap f g = f . g
-}
-- (4) Others -----------------------------------------------------------------

--const :: a -> b -> a st const a x = a is predefined

grd :: (a -> Bool) -> a -> Either a a
grd p x = if p x then Left x else Right x

-- (5) Natural isomorphisms ----------------------------------------------------

swap :: (a,b) -> (b,a)
swap = split snd fst

assocr :: ((a,b),c) -> (a,(b,c))
assocr = split ( fst . fst ) (split ( snd . fst ) snd )

assocl :: (a,(b,c)) -> ((a,b),c)
assocl = split ( id >< p1 ) ( p2 . p2 )

undistr :: Either (a,b) (a,c) -> (a,Either b c)
undistr = either ( id >< i1 ) ( id >< i2 )

undistl :: Either (b, c) (a, c) -> (Either b a, c)
undistl = either ( i1 >< id ) ( i2 >< id )

flatr :: (a,(b,c)) -> (a,b,c)
flatr (a,(b,c)) = (a,b,c)

flatl :: ((a,b),c) -> (a,b,c)
flatl ((b,c),d) = (b,c,d)

pwnil :: a -> (a,()) -- pwnil means "pair with nil"
pwnil = split id (!)

nilpw :: a -> ((), a)
nilpw = split (!) id

coswap :: Either a b -> Either b a
coswap = either i2 i1

coassocr :: Either (Either a b) c -> Either a (Either b c)
coassocr = either (id -|- i1) (i2 . i2)

coassocl :: Either b (Either a c) -> Either (Either b a) c
coassocl = either (i1.i1) (i2 -|- id)

-- (6) Class bifunctor ---------------------------------------------------------

class BiFunctor f where
      bmap :: (a -> b) -> (c -> d) -> (f a c -> f b d)

instance BiFunctor Either where
    bmap f g = f -|- g

instance BiFunctor (,) where
    bmap f g  = f >< g

-- (7) Monads: -----------------------------------------------------------------

-- (7.1) Kleisli monadic composition -------------------------------------------

infixr 4  .!

(.!) :: Monad a => (b -> a c) -> (d -> a b) -> d -> a c
(f .! g) a = (g a) >>= f

mult :: (Monad m) => m (m b) -> m b
-- also known as join
mult = (>>= id)

-- (7.2) Monadic binding ---------------------------------------------------------

ap' :: (Monad m) => (a -> m b, m a) -> m b
ap' = uncurry (flip (>>=))

-- (7.3) Strong monads -----------------------------------------------------------

class (Functor f, Monad f) => Strong f where
      rstr :: (f a,b) -> f(a,b)
      rstr(x,b) = do a <- x ; return (a,b)
      lstr :: (b,f a) -> f(b,a)
      lstr(b,x) = do a <- x ; return (b,a)  

instance Strong IO

instance Strong []
{-- where
    rstr(l,x) = [ (a,x) | a <- l]
    lstr(x,l) = [ (x,a) | a <- l]

--}

instance Strong Identity

instance Strong Maybe
   where
    rstr(Nothing,_) = Nothing
    rstr(Just l,r) = Just(l,r)
    lstr(_,Nothing) = Nothing
    lstr(l,Just r) = Just(l,r)
    
--------------------------------------------------------------------------------
-- Commutative Monads
    
class (Strong f) => Commutative f where
      dstr :: (f a, f b) -> f (a, b)
      -- dstr = rstr .! lstr
      dstr = lstr .! rstr
      
instance Commutative Maybe
instance Commutative IO

-- (7.4) Monad transformers ------------------------------------------------------

--class (Monad m, Monad (t m))  => MT t m where   -- monad transformer class
--      lift :: m a -> t m a

-- nested lifting:

--dlift :: (MT t (t1 m), MT t1 m) => m a -> t (t1 m) a
--dlift = lift . lift

--
