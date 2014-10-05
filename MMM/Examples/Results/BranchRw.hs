module BranchRw where 


import MMM.Core.All
{-# LINE 2 "obj_branchrw.mmm" #-}
set_n_BranchRw :: (Strong m) => (MMM m Integer Integer ())
set_n_BranchRw = (return . (split p2 bang))


{-# LINE 2 "obj_branchrw.mmm" #-}
get_n_BranchRw :: (Strong m) => (MMM m Integer () Integer)
get_n_BranchRw = ((f2m id) .! getst)


{-# LINE 4 "obj_branchrw.mmm" #-}
branchrwRUN :: (Strong m) => (MMM m Integer Integer Integer)
branchrwRUN = (f2m (p1 . _a6))
              .! (mcond
                  (p2 . p2)
                  ((f2m _c5)
                   .! (runm_ set_n_BranchRw)
                      .! (f2m _a4)
                      .! (runm (f2m (\ _pure2 -> (_pure2 + 1))))
                         .! (f2m _a3)
                   .! (runm get_n_BranchRw)
                      .! (f2m _a2))
                  ((f2m (split (split (split (p1 . p2) (p1 . p1)) (p2 . p1)) (p2 . p2)))
                   .! copy))
              .! (runm (f2m (\ (_pure1 , m) -> (_pure1 <= m))))
                 .! (f2m _a1)
              .! (runm get_n_BranchRw)
                 .! (f2m _a0)
              .! (f2m nilpw)
  where
    -- (NIL, m)
    --   -> (NIL, m)
    _a0 = (split p1 p2)
    

    -- (NIL, (m, _pure1))
    --   -> ((_pure1, m), NIL)
    _a1 = (split (split (p2 . p2) (p1 . p2)) (id . p1))
    

    -- ((_pure1, m), (NIL, _parmlin0))
    --   -> (NIL, ((_pure1, m), _parmlin0))
    _a2 = (split (p1 . p2) (split (split (p1 . p1) (p2 . p1)) (p2 . p2)))
    

    -- (NIL, (((_pure1, m), _parmlin0), _pure2))
    --   -> (_pure2, (((NIL, _pure1), m), _parmlin0))
    _a3 = (split (p2 . p2) (split (split (split (id . p1) (((p1 . p1) . p1) . p2)) ((p2 . (p1 . p1)) . p2)) ((p2 . p1) . p2)))
    

    -- (_pure2, ((((NIL, _pure1), m), _parmlin0), _liftset3))
    --   -> (_liftset3, ((((_pure2, NIL), _pure1), m), _parmlin0))
    _a4 = (split (p2 . p2) (split (split (split (split (id . p1) ((((p1 . p1) . p1) . p1) . p2)) ((p2 . ((p1 . p1) . p1)) . p2)) ((p2 . (p1 . p1)) . p2)) ((p2 . p1) . p2)))
    

    -- (_liftset3, ((((_pure2, NIL), _pure1), m), _parmlin0))
    --   -> (((NIL, _pure1), m), _parmlin0)
    _c5 = (p2 . (split (split (id . p1) ((((p1 . p1) . p1) . p1) . p2)) (split (split (split ((p2 . ((p1 . p1) . p1)) . p2) ((p2 . (p1 . p1)) . p2)) ((p2 . p1) . p2)) (p2 . p2))))
    

    -- (((NIL, _pure1), m), _parmlin0)
    --   -> (m, ((NIL, _pure1), _parmlin0))
    _a6 = (split (p2 . p1) (split (split ((p1 . p1) . p1) (p2 . (p1 . p1))) p2))
    



