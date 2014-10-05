module Stack where 


import MMM.Core.All
{-# LINE 2 "obj_stack.mmm" #-}
set_st_Stack :: (Strong m) => (MMM m [Integer] [Integer] ())
set_st_Stack = (return . (split p2 bang))


{-# LINE 2 "obj_stack.mmm" #-}
get_st_Stack :: (Strong m) => (MMM m [Integer] () [Integer])
get_st_Stack = ((f2m id) .! getst)


{-# LINE 4 "obj_stack.mmm" #-}
stackPUSH :: (Strong m) => (MMM m [Integer] Integer ())
stackPUSH = (f2m (p1 . _a3))
            .! (runm_ set_st_Stack)
               .! (f2m _a2)
            .! (runm (f2m (\ (i , _pure4) -> ((:) i _pure4))))
               .! (f2m _a1)
            .! (runm get_st_Stack)
               .! (f2m _a0)
            .! (f2m nilpw)
  where
    -- (NIL, i)
    --   -> (NIL, i)
    _a0 = (split p1 p2)
    

    -- (NIL, (i, _pure4))
    --   -> ((i, _pure4), NIL)
    _a1 = (split (split (p1 . p2) (p2 . p2)) (id . p1))
    

    -- ((i, _pure4), (NIL, _liftset9))
    --   -> (_liftset9, ((i, _pure4), NIL))
    _a2 = (split (p2 . p2) (split (split (p1 . p1) (p2 . p1)) (p1 . p2)))
    

    -- (_liftset9, ((i, _pure4), NIL))
    --   -> (NIL, ((_liftset9, i), _pure4))
    _a3 = (split (p2 . p2) (split (split (id . p1) ((p1 . p1) . p2)) ((p2 . p1) . p2)))
    



stackTOP :: (Strong m) => (MMM m [Integer] () Integer)
stackTOP = (f2m (p1 . _a8))
           .! (mcond
               ((p2 . p1) . p2)
               ((f2m ((split (split (split (split (id . p1) (((p1 . p1) . p1) . p2)) ((p2 . (p1 . p1)) . p2)) ((p2 . p1) . p2)) (p2 . p2)) . ((p2 >< id) . (split (split ((p2 . p1) . p2) (p2 . p2)) (split (split (split (id . p1) ((((p1 . p1) . p1) . p1) . p2)) ((p2 . ((p1 . p1) . p1)) . p2)) ((p2 . (p1 . p1)) . p2))))))
                .! (runm (f2m (const 0)))
                .! (f2m _a4))
               ((f2m (split (split (split (split (((p1 . p1) . p1) . p1) (p2 . ((p1 . p1) . p1))) (p2 . (p1 . p1))) (p2 . p1)) p2))
                .! (f2m _c7)
                   .! (f2m ((split (split (split (split (split (id . p1) ((((p1 . p1) . p1) . p1) . p2)) ((p2 . ((p1 . p1) . p1)) . p2)) ((p2 . (p1 . p1)) . p2)) ((p2 . p1) . p2)) (p2 . p2)) . ((p2 >< id) . (split (split ((p2 . p1) . p2) (p2 . p2)) (split (split (split (split (id . p1) (((((p1 . p1) . p1) . p1) . p1) . p2)) ((p2 . (((p1 . p1) . p1) . p1)) . p2)) ((p2 . ((p1 . p1) . p1)) . p2)) ((p2 . (p1 . p1)) . p2))))))
                      .! (runm (f2m (\ _pure6 -> (head _pure6))))
                      .! (f2m _a6)
                   .! (runm get_st_Stack)
                      .! (f2m _a5)))
           .! (runm (f2m (const 0)))
              .! (f2m _a3)
           .! (runm (f2m (\ _explin2 -> (_explin2 == 0))))
              .! (f2m _a2)
           .! (runm (f2m (\ _pure5 -> (length _pure5))))
              .! (f2m _a1)
           .! (runm1 get_st_Stack)
              .! (f2m _a0)
           .! (f2m id)
  where
    -- NIL
    --   -> (NIL,)
    _a0 = id
    

    -- (NIL, _pure5)
    --   -> (_pure5, NIL)
    _a1 = (split (id . p2) (id . p1))
    

    -- (_pure5, (NIL, _explin2))
    --   -> (_explin2, (_pure5, NIL))
    _a2 = (split (p2 . p2) (split (id . p1) (p1 . p2)))
    

    -- (_explin2, ((_pure5, NIL), _parmlin0))
    --   -> (NIL, ((_explin2, _pure5), _parmlin0))
    _a3 = (split ((p2 . p1) . p2) (split (split (id . p1) ((p1 . p1) . p2)) (p2 . p2)))
    

    -- (NIL, (((_explin2, _pure5), _parmlin0), res))
    --   -> (NIL, (((_explin2, _pure5), _parmlin0), res))
    _a4 = (split (id . p1) (split (split (split (((p1 . p1) . p1) . p2) ((p2 . (p1 . p1)) . p2)) ((p2 . p1) . p2)) (p2 . p2)))
    

    -- (NIL, (((_explin2, _pure5), _parmlin0), res))
    --   -> (NIL, (((_explin2, _pure5), _parmlin0), res))
    _a5 = (split (id . p1) (split (split (split (((p1 . p1) . p1) . p2) ((p2 . (p1 . p1)) . p2)) ((p2 . p1) . p2)) (p2 . p2)))
    

    -- (NIL, ((((_explin2, _pure5), _parmlin0), res), _pure6))
    --   -> (_pure6, ((((NIL, _explin2), _pure5), _parmlin0), res))
    _a6 = (split (p2 . p2) (split (split (split (split (id . p1) ((((p1 . p1) . p1) . p1) . p2)) ((p2 . ((p1 . p1) . p1)) . p2)) ((p2 . (p1 . p1)) . p2)) ((p2 . p1) . p2)))
    

    -- (((((res, _pure6), NIL), _explin2), _pure5), _parmlin0)
    --   -> ((((res, NIL), _explin2), _pure5), _parmlin0)
    _c7 = (p2 . (split (p2 . (((p1 . p1) . p1) . p1)) (split (split (split (split ((((p1 . p1) . p1) . p1) . p1) (p2 . ((p1 . p1) . p1))) (p2 . (p1 . p1))) (p2 . p1)) p2)))
    

    -- ((((res, NIL), _explin2), _pure5), _parmlin0)
    --   -> (res, (((NIL, _explin2), _pure5), _parmlin0))
    _a8 = (split (((p1 . p1) . p1) . p1) (split (split (split (p2 . ((p1 . p1) . p1)) (p2 . (p1 . p1))) (p2 . p1)) p2))
    



stackPOP :: (Strong m) => (MMM m [Integer] () ())
stackPOP = (f2m (p1 . _a7))
           .! (mcond
               (p2 . p2)
               ((f2m _c6)
                .! (runm_ set_st_Stack)
                   .! (f2m _a5)
                   .! (runm (f2m (\ _pure8 -> (tail _pure8))))
                      .! (f2m _a4)
                .! (runm get_st_Stack)
                   .! (f2m _a3))
               ((f2m (split (split (split ((p2 . p1) . p2) (id . p1)) ((p1 . p1) . p2)) (p2 . p2)))
                .! copy))
           .! (runm (f2m (\ _explin3 -> (_explin3 > 0))))
              .! (f2m _a2)
           .! (runm (f2m (\ _pure7 -> (length _pure7))))
              .! (f2m _a1)
           .! (runm1 get_st_Stack)
              .! (f2m _a0)
           .! (f2m id)
  where
    -- NIL
    --   -> (NIL,)
    _a0 = id
    

    -- (NIL, _pure7)
    --   -> (_pure7, NIL)
    _a1 = (split (id . p2) (id . p1))
    

    -- (_pure7, (NIL, _explin3))
    --   -> (_explin3, (_pure7, NIL))
    _a2 = (split (p2 . p2) (split (id . p1) (p1 . p2)))
    

    -- (_explin3, ((_pure7, NIL), _parmlin1))
    --   -> (NIL, ((_explin3, _pure7), _parmlin1))
    _a3 = (split ((p2 . p1) . p2) (split (split (id . p1) ((p1 . p1) . p2)) (p2 . p2)))
    

    -- (NIL, (((_explin3, _pure7), _parmlin1), _pure8))
    --   -> (_pure8, (((NIL, _explin3), _pure7), _parmlin1))
    _a4 = (split (p2 . p2) (split (split (split (id . p1) (((p1 . p1) . p1) . p2)) ((p2 . (p1 . p1)) . p2)) ((p2 . p1) . p2)))
    

    -- (_pure8, ((((NIL, _explin3), _pure7), _parmlin1), _liftset10))
    --   -> (_liftset10, ((((_pure8, NIL), _explin3), _pure7), _parmlin1))
    _a5 = (split (p2 . p2) (split (split (split (split (id . p1) ((((p1 . p1) . p1) . p1) . p2)) ((p2 . ((p1 . p1) . p1)) . p2)) ((p2 . (p1 . p1)) . p2)) ((p2 . p1) . p2)))
    

    -- (_liftset10, ((((_pure8, NIL), _explin3), _pure7), _parmlin1))
    --   -> (((NIL, _explin3), _pure7), _parmlin1)
    _c6 = (p2 . (split (split (id . p1) ((((p1 . p1) . p1) . p1) . p2)) (split (split (split ((p2 . ((p1 . p1) . p1)) . p2) ((p2 . (p1 . p1)) . p2)) ((p2 . p1) . p2)) (p2 . p2))))
    

    -- (((NIL, _explin3), _pure7), _parmlin1)
    --   -> (NIL, ((_explin3, _pure7), _parmlin1))
    _a7 = (split ((p1 . p1) . p1) (split (split (p2 . (p1 . p1)) (p2 . p1)) p2))
    



