{-# LANGUAGE NPlusKPatterns        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
-- |The component combinators are defined in this module.
module MMM.Core.CompComb where

import MMM.Core.FuncComb

--------------------------------------------------------------------------------
-- * Mealy Machines

type MMM m s i o = (s, i) -> m (s, o)

-- |Lifts a function to a machine.
f2m :: (Monad m) => (a -> b) -> MMM m s a b
f2m f = return . (id >< f)

-- |Identity of 'seqc' (and 'cqes')
copy :: (Monad m) => MMM m s a a
copy = f2m id

--------------------------------------------------------------------------------
-- ** State Extensions

-- |Adds a "hole" to the right of @s@'s state.
extr :: (Strong m, Monad m) => MMM m s i o -> MMM m (s, r) i o
extr s = fmap xr . rstr . (s >< id) . xr

-- |Adds a "hole" to the left of @s@'s state.
extl :: (Strong m, Monad m) => MMM m s i o -> MMM m (r, s) i o
extl s = fmap assocl . lstr . (id >< s) . assocr

--------------------------------------------------------------------------------
-- ** Composition

-- |Sequential composition
seqc :: (Strong m, Monad m) =>
        -- input machines
        MMM m s i j ->
        MMM m r j k ->
        -- output machine
        MMM m (s, r) i k
seqc p q = extl q .! extr p

-- |Sequential composition, backwards.
cqes :: (Strong m, Monad m) =>
        -- input machines
        MMM m s j k ->
        MMM m r i j ->
        -- output machine
        MMM m (s, r) i k
cqes p q = extr p .! extl q

--------------------------------------------------------------------------------
-- ** Independent-state Operations

-- |Independent-state Multiplication
times :: (Strong m, Monad m) =>
        -- input machines
        ((s,i) -> m (s, o)) ->
        ((t,j) -> m (t, r)) ->
        -- output machine
        ((s,t), (i,j)) -> m ((s,t), (o,r))
times p q  = fmap m . deltal . (p >< q) . m
  where
    m ((a,b), (i,j)) = ((a,i), (b,j))

-- |Independent-state Addition
(|+|) :: (Strong m, Monad m) =>
           -- input machines
           MMM m s i o ->
           MMM m t j r ->
           -- output
           MMM m (s, t) (Either i j) (Either o r)
p |+| q = (extr p) `sum2` (extl q)
        
--------------------------------------------------------------------------------
-- ** Shared-state Operations   

-- |Shared-state addition.
sum2 :: (Strong m, Monad m) =>
           -- input machines
           MMM m s i j ->
           MMM m s r o ->
           -- output
           MMM m s (Either i r) (Either j o)     
sum2 p q = fmap undr . codelta . (p -|- q) . dr

-- |MMM-flavored split.
msplit :: (Strong m, Monad m) =>
       -- input machines
       MMM m s i j ->
       MMM m s i k ->
       -- output machine
       MMM m s i (j, k)
msplit p q = fmap ((id >< swap) . assocr) . mult 
           . fmap (rstr . (q >< id) . xr) 
           . rstr . (p >< id) . (id `split` p2)

-- |Shared-state Multiplication. Note that this operator is NOT commutative.
(|*|) :: (Strong m, Monad m) =>
       -- input machines
       MMM m s i j ->
       MMM m s k l ->
       -- output machine
       MMM m s (i, k) (j, l)
p |*| q = msplit (wrap p p1 id) (wrap q p2 id)
    
-- |Partial Feedback
pfeedb :: (Functor m, Monad m) =>
          -- input
          MMM m s (Either i z) (Either o z) ->
          -- output
          MMM m s (Either i z) (Either o z)
pfeedb p = ((either return p) . dr . (id >< (i1 -|- i2))) .! p

-- |Wrapping
wrap :: (Functor m) =>
        ((b, e) -> m (a, c)) ->
        (i -> e) ->
        (c -> d) ->
        (b,i) -> m (a, d)
wrap p wi wo = fmap (id >< wo) . p . (id >< wi)

--------------------------------------------------------------------------------
-- ** Branching

mcond :: (Strong m) =>
         (i -> Bool)   ->
         (MMM m s i o) ->
         (MMM m s i o) ->
         MMM m s i o
mcond c th el = f2m idelta .! sum2 th el .! f2m (grd c)

--------------------------------------------------------------------------------
-- ** Input Management

runm :: (Strong m) =>
        MMM m s i o ->
        MMM m s (i, b) (i, (b, o))
runm p = f2m assocr .! msplit copy (p .! f2m p1)

runm1 :: (Strong m) =>
        MMM m s i o ->
        MMM m s i (i, o)
runm1 p = msplit copy p

runm_ :: (Strong m) =>
         MMM m s i o ->
         MMM m s (i, b) (i, b)
runm_ p = f2m (p1 . assocl) .! runm p

--------------------------------------------------------------------------------
-- ** State Management

getst :: (Strong m) => (MMM m s i s)
getst = return . split id id . p1

--------------------------------------------------------------------------------
-- ** Final Coalgebra

-- TODO: Implement the (generalized) functions from Rut06.pdf

-- Def. 2.1
io :: (Monad m) => ([a] -> m [b]) -> a -> m b  -- initial output
io f a = f [a] >>= return . head

fd :: (Monad m) => ([a] -> m [b]) -> a -> [a] -> m [b] -- functional stream derivative
fd f a = (\s -> f (a : s) >>= return . tail)


-- pi coalgebra (Mealy machine, deterministic)
pic :: (Monad m) => ([a] -> m [b]) -> a -> (m b, [a] -> m [b])
pic f a = (io f a, fd f a)


--- define h etc

-- Extension for MMM:
singl x = [x]


final :: (Monad m) => ([i] -> m [o], i) -> ([i] -> m [o], m o)
final = swap . (uncurry pic)


fcoalg :: (Monad m) => ([i] -> m [o]) -> i -> ([i] -> m [o], m o)
fcoalg = curry final


mexpn :: (Monad m) => (b -> c) -> (a -> m b) -> a -> m c
mexpn k f a = f a >>= return . k

{-
unfcoalg :: (Monad m) => (i -> ([i] -> m [o], m o)) -> [i] -> m [o]
unfcoalg f = aux (swap . f) . out
             where aux = uncurry either . (mexpn singl >< uncurry) . unsplit
             
                   unsplit f = (p1.f,p2.f)
                   
                   out [a]   = i1 a
                   out (a:x) = i2 (a,x)
-}
unfcoalg :: (i -> ([i] -> [o], o)) -> [i] -> [o]
unfcoalg f = aux (swap.f) . out
             where aux = uncurry either . (expn singl >< uncurry).unsplit
                   unsplit f = (p1.f,p2.f)
                   out [a]=i1 a
                   out(a:x)=i2(a,x)

c :: (Monad m) => s -> i -> m (s, o)
c = undefined 

ana :: (Monad m) => (s -> i -> m (s, o)) -> s -> [i] -> m [o]
ana = undefined

{-
ana :: (Monad m) => (s -> i -> m (s, o)) -> s -> [i] -> m [o]
ana c = unfcoalg.(mexpn ((ana c) >< id)).c
-}

--------------------------------------------------------------------------------
-- * Auxiliar Functions

deltar :: (Monad m, Strong m) => (m a, m b) -> m (a, b)
deltar = rstr .! lstr

deltal :: (Monad m, Strong m) => (m b, m a) -> m (b, a)
deltal = lstr .! rstr

idelta :: Either b b -> b
idelta = either id id

codelta :: (Functor f) => Either (f a) (f b) -> f (Either a b)
codelta = either (fmap Left) (fmap Right)

xr :: ((a, b), c) -> ((a, c), b)
xr ((u, u'), i) = ((u, i), u')

xl :: ((a, b), c) -> (a, (c, b))
xl ((u, u'), i) = (u, (i, u'))

dr :: (a, Either c b) -> Either (a, c) (a, b)
dr (a, Left x)  = Left (a, x)
dr (a, Right x) = Right (a, x)

undr :: Either (a,b) (a,c) -> (a, Either b c)
undr = either (id >< i1) (id >< i2)

