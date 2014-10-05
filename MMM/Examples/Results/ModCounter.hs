module ModCounter where 


import MMM.Core.All
{-# LINE 2 "obj_modcounter.mmm" #-}
set_m_ModCounter :: (Strong m) => (MMM m (Integer, Integer) Integer ())
set_m_ModCounter = (return . (split (split p2 (p2 . p1)) bang))


{-# LINE 3 "obj_modcounter.mmm" #-}
set_c_ModCounter :: (Strong m) => (MMM m (Integer, Integer) Integer ())
set_c_ModCounter = (return . (split (split (p1 . p1) p2) bang))


{-# LINE 2 "obj_modcounter.mmm" #-}
get_m_ModCounter :: (Strong m) => (MMM m (Integer, Integer) () Integer)
get_m_ModCounter = ((f2m p1) .! getst)


{-# LINE 3 "obj_modcounter.mmm" #-}
get_c_ModCounter :: (Strong m) => (MMM m (Integer, Integer) () Integer)
get_c_ModCounter = ((f2m p2) .! getst)


{-# LINE 5 "obj_modcounter.mmm" #-}
modcounterTICK :: (Strong m) => (MMM m (Integer, Integer) Integer ())
modcounterTICK = (f2m (p1 . _a10))
                 .! (mcond
                     (p2 . p2)
                     ((f2m _c5)
                      .! (runm_ set_c_ModCounter)
                         .! (f2m _a4)
                         .! (runm (f2m (const 0)))
                            .! (f2m _a3))
                     ((f2m (split (split (split (split (((p1 . p1) . p1) . p1) (p2 . ((p1 . p1) . p1))) (p2 . (p1 . p1))) (p2 . p1)) p2))
                      .! (f2m _c9)
                         .! (runm_ set_c_ModCounter)
                            .! (f2m _a8)
                            .! (runm (f2m (\ _pure3 -> (_pure3 + 1))))
                               .! (f2m _a7)
                         .! (runm get_c_ModCounter)
                            .! (f2m _a6)))
                 .! (runm (f2m (\ (_pure1 , _pure2) -> (_pure1 <= _pure2))))
                    .! (f2m _a2)
                 .! (runm get_c_ModCounter)
                    .! (f2m _a1)
                 .! (runm get_m_ModCounter)
                    .! (f2m _a0)
                 .! (f2m nilpw)
  where
    -- (NIL, t)
    --   -> (NIL, t)
    _a0 = (split p1 p2)
    

    -- (NIL, (t, _pure1))
    --   -> (NIL, (t, _pure1))
    _a1 = (split (id . p1) (split (p1 . p2) (p2 . p2)))
    

    -- (NIL, ((t, _pure1), _pure2))
    --   -> ((_pure1, _pure2), (NIL, t))
    _a2 = (split (split ((p2 . p1) . p2) (p2 . p2)) (split (id . p1) ((p1 . p1) . p2)))
    

    -- ((_pure1, _pure2), ((NIL, t), _parmlin0))
    --   -> (NIL, (((_pure1, _pure2), t), _parmlin0))
    _a3 = (split ((p1 . p1) . p2) (split (split (split (p1 . p1) (p2 . p1)) ((p2 . p1) . p2)) (p2 . p2)))
    

    -- (NIL, ((((_pure1, _pure2), t), _parmlin0), _liftset4))
    --   -> (_liftset4, ((((NIL, _pure1), _pure2), t), _parmlin0))
    _a4 = (split (p2 . p2) (split (split (split (split (id . p1) ((((p1 . p1) . p1) . p1) . p2)) ((p2 . ((p1 . p1) . p1)) . p2)) ((p2 . (p1 . p1)) . p2)) ((p2 . p1) . p2)))
    

    -- (_liftset4, ((((NIL, _pure1), _pure2), t), _parmlin0))
    --   -> ((((NIL, _pure1), _pure2), t), _parmlin0)
    _c5 = (p2 . (split (id . p1) (split (split (split (split ((((p1 . p1) . p1) . p1) . p2) ((p2 . ((p1 . p1) . p1)) . p2)) ((p2 . (p1 . p1)) . p2)) ((p2 . p1) . p2)) (p2 . p2))))
    

    -- ((_pure1, _pure2), ((NIL, t), _parmlin0))
    --   -> (NIL, (((_pure1, _pure2), t), _parmlin0))
    _a6 = (split ((p1 . p1) . p2) (split (split (split (p1 . p1) (p2 . p1)) ((p2 . p1) . p2)) (p2 . p2)))
    

    -- (NIL, ((((_pure1, _pure2), t), _parmlin0), _pure3))
    --   -> (_pure3, ((((NIL, _pure1), _pure2), t), _parmlin0))
    _a7 = (split (p2 . p2) (split (split (split (split (id . p1) ((((p1 . p1) . p1) . p1) . p2)) ((p2 . ((p1 . p1) . p1)) . p2)) ((p2 . (p1 . p1)) . p2)) ((p2 . p1) . p2)))
    

    -- (_pure3, (((((NIL, _pure1), _pure2), t), _parmlin0), _liftset5))
    --   -> (_liftset5, (((((_pure3, NIL), _pure1), _pure2), t), _parmlin0))
    _a8 = (split (p2 . p2) (split (split (split (split (split (id . p1) (((((p1 . p1) . p1) . p1) . p1) . p2)) ((p2 . (((p1 . p1) . p1) . p1)) . p2)) ((p2 . ((p1 . p1) . p1)) . p2)) ((p2 . (p1 . p1)) . p2)) ((p2 . p1) . p2)))
    

    -- (_liftset5, (((((_pure3, NIL), _pure1), _pure2), t), _parmlin0))
    --   -> ((((NIL, _pure1), _pure2), t), _parmlin0)
    _c9 = (p2 . (split (split (id . p1) (((((p1 . p1) . p1) . p1) . p1) . p2)) (split (split (split (split ((p2 . (((p1 . p1) . p1) . p1)) . p2) ((p2 . ((p1 . p1) . p1)) . p2)) ((p2 . (p1 . p1)) . p2)) ((p2 . p1) . p2)) (p2 . p2))))
    

    -- ((((NIL, _pure1), _pure2), t), _parmlin0)
    --   -> (NIL, (((_pure1, _pure2), t), _parmlin0))
    _a10 = (split (((p1 . p1) . p1) . p1) (split (split (split (p2 . ((p1 . p1) . p1)) (p2 . (p1 . p1))) (p2 . p1)) p2))
    



