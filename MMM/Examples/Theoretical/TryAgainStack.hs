module MMM.Examples.Theoretical.TryAgainStack where

import MMM.Core.All hiding (pfeedb)
import MMM.Qais
import MMM.Examples.Theoretical.Folder (stack)
import Control.Monad (join)

delta :: (Strong m)
      => ((s , i) -> (s , Either i ()))
      -> MMM m s (Either i s) (Either i ())
delta test = return
           . (either id (split id (i2 . bang))) 
           . (test -|- p2) 
           . dr

-- Here we define a slightly different version of our feedback combinator.
feedb :: (Strong m)
      => MMM m s (Either i z) (Either (z , o) p)
      -> MMM m s i (Either o p)
feedb p = join 
        . fmap ( (fmap undr)
               . (either (fmap i1) (fmap i2))
               . (rstr -|- return)
               . ((fmap p1 >< id) -|- id)
               . ((p >< id) -|- id)
               . (((id >< i2) >< id) -|- id)
               . ((assocl -|- id) . dr)
        ) . p . (id >< i1)
        
-- A simple mirror combinator, to contract the state into two states.
mirror :: (Strong m)
       => MMM m s i o
       -> MMM m s i (s , o)
mirror p = fmap (assocr . ((split id id) >< id)) . p

-- An idle MMM
idle :: (Strong m) => MMM m s () ()
idle = f2m id
        
-- And finally, the tryagain combinator that,
-- given a test t and a machine p runs p as a partial machine with regard to t.
tryagain :: (Strong m)
         => ((s , i) -> (s , Either i ()))
         -> MMM m s i o
         -> MMM m (s , s) i (Either o ())
tryagain t p = feedb (delta t `seqc` (mirror p `sum2` idle))

-- As a simple example, we can totalize our stack machine in this fashion!

-- The condition is deterministic, that is, no behaviour monad here.
-- (or, an MMM Identity s i o)
noEmptyPop :: ([p] , Either () p) -> ([p] , Either (Either () p) ())
noEmptyPop ([] , Left _) = ([] , Right ())
noEmptyPop (l , i)       = (l , Left i)

-- Note the duplication of the state with diagonal, that's needed to pass
-- a copy to (delta noEmptyPop).
safeStack :: (Strong m) => MMM m [p] (Either () p) (Either (Either p ()) ())
safeStack = fmap (p2 >< id) . tryagain noEmptyPop stack . (diagonal >< id)
  where diagonal a = (a , a)



