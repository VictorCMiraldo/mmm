module Branching where 


import MMM.Core.All
{-# LINE 2 "obj_branch.mmm" #-}
set_tr_Branching :: (Strong m) => (MMM m Integer Integer ())
set_tr_Branching = (return . (split p2 bang))


{-# LINE 2 "obj_branch.mmm" #-}
get_tr_Branching :: (Strong m) => (MMM m Integer () Integer)
get_tr_Branching = ((f2m id) .! getst)


{-# LINE 4 "obj_branch.mmm" #-}
branchingF :: (Strong m) => (MMM m Integer Integer Integer)
branchingF = (f2m (p1 . _a4))
             .! (mcond
                 (p2 . p2)
                 ((runm (f2m (const 0)))
                  .! (f2m _a2))
                 ((f2m (split (id . p1) (split (split (split (((p1 . p1) . p1) . p2) ((p2 . (p1 . p1)) . p2)) ((p2 . p1) . p2)) (p2 . p2))))
                  .! (runm (f2m (const 1)))
                     .! (f2m _a3)))
             .! (runm (f2m (\ (i , _pure1) -> (i <= _pure1))))
                .! (f2m _a1)
             .! (runm get_tr_Branching)
                .! (f2m _a0)
             .! (f2m nilpw)
  where
    -- (NIL, i)
    --   -> (NIL, i)
    _a0 = (split p1 p2)
    

    -- (NIL, (i, _pure1))
    --   -> ((i, _pure1), NIL)
    _a1 = (split (split (p1 . p2) (p2 . p2)) (id . p1))
    

    -- ((i, _pure1), (NIL, _parmlin0))
    --   -> (NIL, ((i, _pure1), _parmlin0))
    _a2 = (split (p1 . p2) (split (split (p1 . p1) (p2 . p1)) (p2 . p2)))
    

    -- ((i, _pure1), (NIL, _parmlin0))
    --   -> (NIL, ((i, _pure1), _parmlin0))
    _a3 = (split (p1 . p2) (split (split (p1 . p1) (p2 . p1)) (p2 . p2)))
    

    -- (NIL, (((i, _pure1), _parmlin0), a))
    --   -> (a, (((NIL, i), _pure1), _parmlin0))
    _a4 = (split (p2 . p2) (split (split (split (id . p1) (((p1 . p1) . p1) . p2)) ((p2 . (p1 . p1)) . p2)) ((p2 . p1) . p2)))
    



