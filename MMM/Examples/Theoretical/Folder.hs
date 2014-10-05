module MMM.Examples.Folder where

import MMM.Core.All
import MMM.Qais

push :: (Monad m, Strong m) => ([p], p) -> m ([p], ())
push (t, h) = return (h:t, ())
  
pop :: (Monad m, Strong m) => ([p], ()) -> m ([p], p)
pop (l, ()) = return $ (split tail head) l

stack :: (Strong m) => ([p], Either () p) -> m ([p], Either p ())
stack = sum2 pop push

folder :: (Strong m) => (([p], [p]), Either () ()) -> m (([p], [p]), Either () ())
folder = tr `sum2` tl
  where
    tr = pop `seqc` push
    
    tl = push `cqes` pop


