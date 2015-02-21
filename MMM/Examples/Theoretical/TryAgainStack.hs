module MMM.Examples.Theoretical.TryAgainStack where

import MMM.Core.All
import MMM.Qais
import MMM.Examples.Theoretical.Folder (stack)

delta :: (Strong m)
      => ((s, i) -> (s, Either (s , i) ()))
      -> MMM m s (Either i s) (Either (s , i) ())
delta p = return . (either id (split id (i2 . bang))) . (p -|- p2) . dr


idle :: (Strong m) => MMM m s () ()
idle = f2m id


noEmptyPop :: (Strong m) 
           => ([p] , Either () p) 
           -> m ([p] , Either (Either () p) ())
noEmptyPop ([] , Left _) = return ([] , i2 ())
noEmptyPop (l , i)       = return (l , i1 i) 


guardedStack :: (Strong m)
             => ([p] , Either () p)
             -> m ([p] , Either (Either p ()) ())
guardedStack = (stack `sum2` idle) .! noEmptyPop



