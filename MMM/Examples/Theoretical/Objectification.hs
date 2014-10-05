module MMM.Examples.Objectification where

import MMM.Core.All
import MMM.Qais

import qualified Test.QuickCheck as Q

-- In this example we'll use some faulty monadic components
-- to illustrate both the objectification theorem and the
-- construction of such machines using the primitives provided.

--------------------------------------------------------------------------------
-- * Dist and Maybe

rd :: (Int, ()) -> Lift Dist Maybe (Int, Int)
rd (0, _) = lft Nothing
rd x = L . schoice 0.9 rdOk rdZero $ x
  where
    rdOk (i, _)   
      | even i = Just (i-1, i)
      | True   = Just (i, i)
    rdZero (i, _) = Just (i, 0)

wr :: (Int, Int) -> Lift Dist Maybe (Int, Int)
wr (_, 0) = lft Nothing
wr  x = L . schoice 0.99 wrOk wrZero $ x
  where
    wrOk (_, n) = Just (n, n)
    wrZero _    = Just (0, 0)
  
inc :: ((), Int) -> Lift Dist Maybe ((), Int)
inc = L . schoice 0.8 incOk incId
  where
    incOk (_, i) = Just ((), i+1)
    incId (_, i) = Just ((), i)

dec :: ((), Int) -> Lift Dist Maybe ((), Int)
dec ((), i) = return ((), i-1)

seqcOuter = (rd `sum2` wr) `seqc` (inc `sum2` dec)

seqcInner = (rd `seqc` inc) `sum2` (wr `seqc` dec)

testOuterInner = Q.verboseCheck ((\s -> seqcInner s == seqcOuter s) 
              :: ((Int, ()), Either () Int) -> Bool)
              
--------------------------------------------------------------------------------
-- * Dist and Powerset
              
rnd :: (Eq k) => ([k], ()) -> Lift Dist [] ([k], k)
rnd l = L $ D [(rndTake l, 1)]
  where
    rndTake (l, _) = l >>= \x -> return (l \\ [x], x)
    
sq :: (Num k) => ((), k) -> Lift Dist [] ((), k)
sq = L . schoice 0.8 sqOk sqFail
  where
    sqOk (_, k) = return ((), k*k)
    sqFail _    = return ((), -1)
    
cube :: (Num k) => ((), k) -> Lift Dist [] ((), k)
cube = L . schoice 0.8 cubeOk cubeFail
  where
    cubeOk (_, k) = return ((), k*k*k)
    cubeFail _    = return ((), -1)
    
hd :: ([Integer], ()) -> Lift Dist [] ([Integer], Integer)
hd ([], ()) = L $ D [([([], 1)], 1)]
hd (l, ())  = L $ D [([(l, head l)], 1)]
   
seqcInner' = (rnd `seqc` sq) `sum2` (hd `seqc` cube)

seqcOuter' = (rnd `sum2` hd) `seqc` (sq `sum2` cube)
