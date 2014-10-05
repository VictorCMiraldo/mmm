module Folder where 


import MMM.Core.All
import Data.Default(def)
{-# LINE 2 "obj_folder.mmm" #-}
set_sl_Folder :: (Strong m) => (MMM m (Stack, Stack) Stack ())
set_sl_Folder = (return . (split (split p2 (p2 . p1)) bang))


{-# LINE 3 "obj_folder.mmm" #-}
set_sr_Folder :: (Strong m) => (MMM m (Stack, Stack) Stack ())
set_sr_Folder = (return . (split (split (p1 . p1) p2) bang))


{-# LINE 2 "obj_folder.mmm" #-}
get_sl_Folder :: (Strong m) => (MMM m (Stack, Stack) () Stack)
get_sl_Folder = ((f2m p1) .! getst)


{-# LINE 3 "obj_folder.mmm" #-}
get_sr_Folder :: (Strong m) => (MMM m (Stack, Stack) () Stack)
get_sr_Folder = ((f2m p2) .! getst)


{-# LINE 5 "obj_folder.mmm" #-}
folder :: (Strong m) => (Stack -> ((m (Stack, Stack))))
folder = ((return . p1) .! ((f2m (p1 . _a3))
                            .! (runm_ set_sr_Folder)
                               .! (f2m _a2)
                            .! (runm (f2m (const [])))
                               .! (f2m _a1)
                            .! (runm_ set_sl_Folder)
                               .! (f2m _a0)
                            .! (f2m nilpw) .! (return . (split (const def) id))))
  where
    -- (NIL, s)
    --   -> (s, NIL)
    _a0 = (split p2 p1)
    

    -- (s, NIL)
    --   -> (NIL, s)
    _a1 = (split (id . p2) (id . p1))
    

    -- (NIL, (s, _liftset3))
    --   -> (_liftset3, (NIL, s))
    _a2 = (split (p2 . p2) (split (id . p1) (p1 . p2)))
    

    -- (_liftset3, (NIL, s))
    --   -> (NIL, (_liftset3, s))
    _a3 = (split (p1 . p2) (split (id . p1) (p2 . p2)))
    



folderTR :: (Strong m) => (MMM m (Stack, Stack) () ())
folderTR = (f2m (p1 . _a5))
           .! (runm_ (extl stackPUSH))
              .! (f2m _a4)
           .! (runm_ (extr stackPOP))
              .! (f2m _a3)
           .! (runm (f2m (\ _pure0 -> _pure0)))
              .! (f2m _a2)
           .! (runm1 (extr stackTOP)
                     .! (f2m _a1))
              .! (f2m _a0)
           .! (f2m id)
  where
    -- NIL
    --   -> NIL
    _a0 = id
    

    -- NIL
    --   -> NIL
    _a1 = id
    

    -- (NIL, _pure0)
    --   -> (_pure0, NIL)
    _a2 = (split p2 p1)
    

    -- (_pure0, (NIL, p))
    --   -> (NIL, (_pure0, p))
    _a3 = (split (p1 . p2) (split (id . p1) (p2 . p2)))
    

    -- (NIL, (_pure0, p))
    --   -> (p, (NIL, _pure0))
    _a4 = (split (p2 . p2) (split (id . p1) (p1 . p2)))
    

    -- (p, (NIL, _pure0))
    --   -> (NIL, (p, _pure0))
    _a5 = (split (p1 . p2) (split (id . p1) (p2 . p2)))
    



folderTL :: (Strong m) => (MMM m (Stack, Stack) () ())
folderTL = (f2m (p1 . _a5))
           .! (runm_ (extr stackPUSH))
              .! (f2m _a4)
           .! (runm_ (extl stackPOP))
              .! (f2m _a3)
           .! (runm (f2m (\ _pure1 -> _pure1)))
              .! (f2m _a2)
           .! (runm1 (extl stackTOP)
                     .! (f2m _a1))
              .! (f2m _a0)
           .! (f2m id)
  where
    -- NIL
    --   -> NIL
    _a0 = id
    

    -- NIL
    --   -> NIL
    _a1 = id
    

    -- (NIL, _pure1)
    --   -> (_pure1, NIL)
    _a2 = (split p2 p1)
    

    -- (_pure1, (NIL, p))
    --   -> (NIL, (_pure1, p))
    _a3 = (split (p1 . p2) (split (id . p1) (p2 . p2)))
    

    -- (NIL, (_pure1, p))
    --   -> (p, (NIL, _pure1))
    _a4 = (split (p2 . p2) (split (id . p1) (p1 . p2)))
    

    -- (p, (NIL, _pure1))
    --   -> (NIL, (p, _pure1))
    _a5 = (split (p1 . p2) (split (id . p1) (p2 . p2)))
    



folderREAD :: (Strong m) => (MMM m (Stack, Stack) () Integer)
folderREAD = (f2m (p1 . _a2))
             .! (runm1 (extl stackTOP)
                       .! (f2m _a1))
                .! (f2m _a0)
             .! (f2m id)
  where
    -- NIL
    --   -> NIL
    _a0 = id
    

    -- NIL
    --   -> NIL
    _a1 = id
    

    -- (NIL, _pure2)
    --   -> (_pure2, NIL)
    _a2 = (split p2 p1)
    



{-# LINE 29 "obj_folder.mmm" #-}
set_st_Stack :: (Strong m) => (MMM m [Integer] [Integer] ())
set_st_Stack = (return . (split p2 bang))


{-# LINE 29 "obj_folder.mmm" #-}
get_st_Stack :: (Strong m) => (MMM m [Integer] () [Integer])
get_st_Stack = ((f2m id) .! getst)


{-# LINE 31 "obj_folder.mmm" #-}
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
    --   -> NIL
    _a0 = id
    

    -- (NIL, _pure5)
    --   -> (_pure5, NIL)
    _a1 = (split p2 p1)
    

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
    --   -> NIL
    _a0 = id
    

    -- (NIL, _pure7)
    --   -> (_pure7, NIL)
    _a1 = (split p2 p1)
    

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
    



type Stack = [Integer]
type Folder = (Stack, Stack)
