module Counter where 


import MMM.Core.All
{-# LINE 2 "obj_counter.mmm" #-}
set_c_Counter :: (Strong m) => (MMM m Integer Integer ())
set_c_Counter = (return . (split p2 bang))


{-# LINE 2 "obj_counter.mmm" #-}
get_c_Counter :: (Strong m) => (MMM m Integer () Integer)
get_c_Counter = ((f2m id) .! getst)


{-# LINE 6 "obj_counter.mmm" #-}
counterINC :: (Strong m) => (MMM m Integer Integer ())
counterINC = (f2m (p1 . _a4))
             .! (f2m (p1 . _a3))
             .! (runm_ set_c_Counter)
                .! (f2m _a2)
             .! (runm (f2m (\ (_pure0 , i) -> (_pure0 + i))))
                .! (f2m _a1)
             .! (runm get_c_Counter)
                .! (f2m _a0)
             .! (f2m nilpw)
  where
    -- (NIL, i)
    --   -> (NIL, i)
    _a0 = (split p1 p2)
    

    -- (NIL, (i, _pure0))
    --   -> ((_pure0, i), NIL)
    _a1 = (split (split (p2 . p2) (p1 . p2)) (id . p1))
    

    -- ((_pure0, i), (NIL, _liftset2))
    --   -> (_liftset2, ((_pure0, i), NIL))
    _a2 = (split (p2 . p2) (split (split (p1 . p1) (p2 . p1)) (p1 . p2)))
    

    -- (_liftset2, ((_pure0, i), NIL))
    --   -> (NIL, ((_liftset2, _pure0), i))
    _a3 = (split (p2 . p2) (split (split (id . p1) ((p1 . p1) . p2)) ((p2 . p1) . p2)))
    

    -- (NIL, ((_liftset2, _pure0), i))
    --   -> (NIL, ((_liftset2, _pure0), i))
    _a4 = (split (id . p1) (split (split ((p1 . p1) . p2) ((p2 . p1) . p2)) (p2 . p2)))
    



{-# LINE 8 "obj_counter.mmm" #-}
counterDEC :: (Strong m) => (MMM m Integer Integer ())
counterDEC = (f2m (p1 . _a4))
             .! (f2m (p1 . _a3))
             .! (runm_ set_c_Counter)
                .! (f2m _a2)
             .! (runm (f2m (\ (_pure1 , i) -> (_pure1 - i))))
                .! (f2m _a1)
             .! (runm get_c_Counter)
                .! (f2m _a0)
             .! (f2m nilpw)
  where
    -- (NIL, i)
    --   -> (NIL, i)
    _a0 = (split p1 p2)
    

    -- (NIL, (i, _pure1))
    --   -> ((_pure1, i), NIL)
    _a1 = (split (split (p2 . p2) (p1 . p2)) (id . p1))
    

    -- ((_pure1, i), (NIL, _liftset3))
    --   -> (_liftset3, ((_pure1, i), NIL))
    _a2 = (split (p2 . p2) (split (split (p1 . p1) (p2 . p1)) (p1 . p2)))
    

    -- (_liftset3, ((_pure1, i), NIL))
    --   -> (NIL, ((_liftset3, _pure1), i))
    _a3 = (split (p2 . p2) (split (split (id . p1) ((p1 . p1) . p2)) ((p2 . p1) . p2)))
    

    -- (NIL, ((_liftset3, _pure1), i))
    --   -> (NIL, ((_liftset3, _pure1), i))
    _a4 = (split (id . p1) (split (split ((p1 . p1) . p2) ((p2 . p1) . p2)) (p2 . p2)))
    



