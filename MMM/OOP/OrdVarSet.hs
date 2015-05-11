-----------------------------------------------------------------------------
-- |
-- Module      :  OrdVarSet
-- Copyright   :  HASLAb team, University of Minho
-- License     :  MIT
--
-- Maintainer  :  Victor Miraldo <victor.cacciari@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
--Provides general utility to handle ORDered VARiable SETs, connecting to
--the report, this module translates the "select" and "rewrite" meta
--combinators to MMMs.
----------------------------------------------------------------------------
module MMM.OOP.OrdVarSet where

import MMM.Core.FuncComb
import MMM.OOP.Language.Syntax(IsVar(..))
import MMM.Util.Util hiding ((<>))
import MMM.Util.Pretty
import MMM.HsMMM

import qualified Data.Map as M

import Control.Monad.State
import Control.Applicative

import Data.Function(on)
import Data.List(sortBy, (\\))
import Data.Maybe(fromJust)

import qualified Debug.Trace as T

--------------------------------------------------------------------------------
-- * Base Structure

-- |A (VarSet id) stores both the variables in scope and their current
--  position in "memory" (or, in the product passed as input to functions).
--
--  To illustrate the functions below, we'll consider a (vs :: VS id) such that
--
--  @vs = M.fromList $ [("a", 1), ("b", 2), ("c", 3), ("d", 4)]@
--
--  Note the distinction between focused and unfocused varsets,
--  Gladly, products are associative, therefore we can shuffle them
--  however we see fit.
data VS id
  -- |A VSSingle represents a normally left-associative product,
  --  If vs is a VSSingle, it represents (((a * b) * c) * d)
  = VSSingle (M.Map id Int)
  -- |A focus point is used to pass inputs to components. If
  --  vs is a VSFocus 1, it will represent (a * ((b * c) * d))
  | VSFocus Int (M.Map id Int)
  deriving (Show, Eq)
  
instance (IsVar id) => Pretty (VS id) where
  pretty vs
    = case vs of
        VSSingle _  -> pprAssocLeftWith pprPair $ vsSortedKeys vs
        VSFocus i _ -> let (f, r) = splitAt i (vsSortedKeys vs)
                       in parens $ pprAssocLeftWith pprPair f <> comma 
                               <+> pprAssocLeftWith pprPair r
    where
      pprPair d a = parens $ d <> comma <+> pretty a
  
vsGetMap :: (IsVar id) => VS id -> M.Map id Int
vsGetMap = vsCata id (const id)
  
--------------------------------------------------------------------------------
-- ** Base Structure Utilities

vsCata :: (IsVar id) 
       => (M.Map id Int -> a) 
       -> (Int -> M.Map id Int -> a)
       -> VS id
       -> a
vsCata single _ (VSSingle m)  = single m
vsCata _ focus  (VSFocus i m) = focus i m

-- |Looks up a variable in the current state.
vsLookup :: (IsVar id) => id -> VS id -> Maybe Int
vsLookup i = vsCata (M.lookup i) (const $ M.lookup i)

-- |Builds a projection for a variable.
vsMkProj :: (IsVar id) => id -> VS id -> Maybe HsExp
vsMkProj v (VSSingle m)  
  = HsEProj (M.size m) . (+1) <$> M.lookup v m
vsMkProj v vs@(VSFocus f m)
  = vsLookup v vs >>= return . mkProj vs
  where
    mkProj (VSFocus f m) i
      | i < f = flip HsEComp (HsEProj 2 1) . HsEProj f . (+1)              $ i
      | True  = flip HsEComp (HsEProj 2 2) . HsEProj (M.size m - f) . (+1) $ i - f
  
-- |Returns the keys of out map
vsKeys :: (IsVar id) => VS id -> [id]
vsKeys (VSSingle m)  = M.keys m
vsKeys (VSFocus _ m) = M.keys m

-- |Returns the keyset of a VS, ordered by their position in memory
vsSortedKeys :: (IsVar id) => VS id -> [id]
vsSortedKeys = vsCata aux (const aux)
  where
    aux = map fst . sortBy (compare `on` snd) . M.toList
  
-- |Appends a new variable, in the end of the map
vsAppTail :: (IsVar id) => id -> VS id -> VS id
vsAppTail v = vsCata (insS v) (insF v)
  where
    insS v m   = VSSingle $ M.insert v (M.size m) m
    insF v f m = VSFocus f $ M.insert v (M.size m) m
    
-- |Appends a new variable, in the beggining of the map
vsAppHead :: (IsVar id) => id -> VS id -> VS id
vsAppHead v = vsCata (ins v) err
  where
    ins v m = VSSingle $ M.insert v 0 $ M.map (+1) m
    err f m = bug (f, m) "OrdVarSet.vsAppHead: Can't append a variable into a focused variable set head."
    
-- *** Data.Map trivial extensions
    
-- |Data.Map.mapKeys extended to VS
vsMapKeys :: (IsVar id, IsVar id') => (id -> id') -> VS id -> VS id'
vsMapKeys h = vsCata (VSSingle . M.mapKeys h) (\f -> VSFocus f . M.mapKeys h)

-- |Data.Map.mapWithKey extended to VS
vsMapWithKey :: (IsVar id) => (id -> Int -> Int) -> VS id -> VS id
vsMapWithKey h = vsCata (VSSingle . M.mapWithKey h) (\f -> VSFocus f . M.mapWithKey h)

-- |Data.Map.size extended to VS
vsSize :: (IsVar id) => VS id -> Int
vsSize = vsCata M.size (const $ M.size)

-- |Data.Map.map extended to VS
vsMap :: (IsVar id) => (Int -> Int) -> VS id -> VS id
vsMap f = vsCata (VSSingle . M.map f) (\i -> VSFocus i . M.map f)

-- |Data.Map.delete extended to VS
vsDelete :: (IsVar id) => id -> VS id -> VS id
vsDelete v = vsCata (VSSingle . M.delete v) (delfocus v)
  where
    delfocus v f m 
      = case M.lookup v m of
          Nothing  -> VSFocus f m
          Just idx -> if idx < f
                      then VSFocus (f-1) (M.delete v m)
                      else VSFocus f     (M.delete v m)
                      
-- |Data.Map.toList extended to VS
vsToList :: (IsVar id) => VS id -> [(id, Int)]
vsToList = vsCata (M.toList) (const $ M.toList)


-- *** Predicates

vsIsSingle, vsIsFocus :: (IsVar id) => VS id -> Bool
vsIsSingle = vsCata (const True) (\_ _ -> False)
vsIsFocus  = not . vsIsSingle

-- *** Generation

-- |Generates a single varset from a list of identifiers.
vsFromList :: (IsVar id) => [id] -> VS id
vsFromList l = VSSingle $ M.fromList $ zip l [0..] 

-- |Generates a focused varset from a list and a focus point.
vsFromListFocus :: (IsVar id) => [id] -> Int -> VS id
vsFromListFocus l i 
  | length l == i = VSSingle (M.fromList $ zip l [0..])
  | otherwise     = VSFocus i $ M.fromList $ zip l [0..] 

  
--------------------------------------------------------------------------------
-- ** State Monad Encapsulation

-- |Encapsulation of our dict into a state.
type OrdVS id = StateT (VS id)

-- |Unwrapping
runOrdVS :: (Monad m) => OrdVS id m a -> m a
runOrdVS = flip evalStateT (VSSingle M.empty)

runOrdVSvb :: (Monad m) => OrdVS id m a -> m (a, VS id)
runOrdVSvb = flip runStateT (VSSingle M.empty)

--------------------------------------------------------------------------------
-- *** Monadic Functionality

-- |Get
ordvsGetVarSet :: (IsVar id, Monad m) => OrdVS id m (VS id)
ordvsGetVarSet = get

-- |Put
ordvsPutVarSet :: (IsVar id, Monad m) => VS id -> OrdVS id m ()
ordvsPutVarSet = put

-- |Pretty prints the varset with the current variable names.
ordvsGetVarSetPretty :: (IsVar id, Monad m) => OrdVS id m String
ordvsGetVarSetPretty = get >>= return . show . pretty

-- |Returns a projection that will select a given variable.
--  @ordvsGetV "c" = Just $ HsEProj 4 3@
ordvsGetV :: (IsVar id, Monad m) => id -> OrdVS id m (Maybe HsExp)
ordvsGetV v = get >>= return . vsMkProj v
            
-- |Same as above, unsafe version.                  
ordvsGetV_U :: (IsVar id, Monad m) => id -> OrdVS id m HsExp
ordvsGetV_U v = ordvsGetV v >>= return . maybe err id
  where
    err = bug v "OrdVarSet.ordvsGetV_U: no such variable."

-- |Returns the list of variables in scope.
ordvsGetVs :: (IsVar id, Monad m) => OrdVS id m [id]
ordvsGetVs = get >>= return . vsSortedKeys

-- |Returns the current focus point
ordvsGetFocus :: (IsVar id, Monad m) => OrdVS id m Int
ordvsGetFocus = get >>= return . vsCata (const 0) (\i _ -> i)

-- |Creates a new variable, in the end of the current var list.
ordvsNewV :: (IsVar id, Monad m) => id -> OrdVS id m ()
ordvsNewV i = modify (vsAppTail i)

-- |Creates a new variable, in the head of the current list.
ordvsNewVhd :: (IsVar id, Monad m) => id -> OrdVS id m ()
ordvsNewVhd i = modify (vsAppHead i)

-- |Adds a "nil" valued variable, or a button in MMM jargon, to
--  the current input. The name of this variable is "NIL".
--  Returns a "pwnil" function.
ordvsAddNIL :: (IsVar id, Monad m) => OrdVS id m HsExp
ordvsAddNIL = ordvsNewVhd (varBuild "NIL") >> return (HsEVar "nilpw")

-- |Given a machine returns the correct running boilerplate,
ordvsRunm :: (IsVar id, Monad m) => HsMMM -> OrdVS id m HsMMM
ordvsRunm m
  = do
    vs <- ordvsGetVs
    return $ if length vs == 2
             then (HsMRunm1 m)
             else (HsMRunm m)
    
-- |Same as above, but uses the runm_, for ignoring output.
ordvsRunm_ :: (IsVar id, Monad m) => HsMMM -> OrdVS id m HsMMM
ordvsRunm_ m = ordvsRunm m
           >>= \x -> return $ case x of
                                  (HsMRunm m') -> HsMRunmI m'
                                  _            -> x

-- |Renames a variable
ordvsRename :: (IsVar id, Monad m) => id -> id -> OrdVS id m ()
ordvsRename on nn = modify (vsMapKeys $ rn on nn)
  where
    rn on nn k
      | k == on   = nn
      | otherwise = k  
      
-- |@ordvsUnifyWith vs@ will return a associativity that, if applied to
--  vs will unify with the current varset. Make sure they're unifiable,
--  that is, they must have the same key set.
ordvsUnifyWith :: (IsVar id, Monad m) => VS id -> OrdVS id m HsExp
ordvsUnifyWith target
  = do
    vs <- get
    -- two var-sets curr and new are unifiable when new \subseteq curr
    when (not $ unifiable (vsKeys vs) (vsKeys target)) 
         $ bug (target, vs) "Can't unify the varsets."
    let projs = map (fromJust . flip vsMkProj vs) $ vsSortedKeys target
    put target
    let i = vsCata (\_ -> 0) (\i _ -> i) target
    return $ buildSplit i projs
  where
    unifiable currVs = all (`elem` currVs)
    
    buildSplit _ [] = HsEId
    buildSplit 0 ls = foldl1 HsESplit ls
    buildSplit i ls = let (focus, rest) = splitAt i ls
                      in case rest of
                          [] -> (foldl1 HsESplit focus)
                          _  -> HsESplit (foldl1 HsESplit focus) (foldl1 HsESplit rest)
    

-- |Performs an assoc operation, by selecting a list of variables and
--  pushing them to the beginning of the list.
--
--  @ordvsAssoc ["d", "c"] = split (split p4 p3) (split p1 p2)@
--
--  Note that after a ordvsAssoc the input type becomes focused. There's
--  more detail on this in the report, but we have to fix it afterwards, otherwise
--  some OrdVS functions might fail. You should call 'ordvsRmFocus' to get back to
--  a single VS.
ordvsAssoc  :: (IsVar id, Monad m) => [id] -> OrdVS id m HsExp
ordvsAssoc [] = ordvsGetV (varBuild "NIL") 
            >>= maybe (ordvsAddNIL >>= return) 
                      (\_ -> ordvsAssoc [varBuild "NIL"])
ordvsAssoc sl
  = do
    ks <- ordvsGetVs
    let sl' = sl `assertSublistAndCompleteWith` ks
    ordvsUnifyWith (vsFromListFocus sl' (length sl))
  where
    assertSublistAndCompleteWith sl ll
      = if sl <=* ll
        then sl ++ (ll \\ sl)
        else bug (sl, ll) "ordvsAssoc argument must be a sublist of the current keyset"

    sl <=* ll = all (`elem` ll) sl

-- |Removes the focus point. Returns the required associativity isomorphism.            
ordvsRmFocus :: (IsVar id, Monad m) => OrdVS id m HsExp
ordvsRmFocus
  = do
    vs <- get
    case vs of
      (VSFocus f m) -> 
        do 
          put (VSSingle m) 
          return $ mkSplit
                 $ map (\(v, _) -> fromJust $ vsMkProj v vs) 
                 $ sortBy (compare `on` snd) $ M.toList m
      vs' -> return HsEId
  where
    mkSplit [] = error "mkSplit"
    mkSplit l  = foldl1 HsESplit l
                                        
-- |Performs a rewrite of two variables. @ordvsRw old new@ will write
--  @new@ on top of @old@. Returns the assoc function needed to perform
--  the operation on MMM level.
ordvsRw :: (IsVar id, Monad m) => id -> id -> OrdVS id m (Maybe HsExp)
ordvsRw old new
  = do
    assoc <- ordvsAssoc [old, new]
    modify (rmAndFix old)
    rmf <- ordvsRmFocus
    return . Just . HsEComp rmf
           . HsEComp (HsEProd [HsEProj 2 2, HsEId]) $ assoc
  where
    rmAndFix :: (IsVar id) => id -> VS id -> VS id
    rmAndFix o (VSFocus f m) 
      = let
        oi = fromJust $ M.lookup o m
        m' = M.map (\v -> if v > oi then v-1 else v) m
      in VSFocus (f-1) $ M.delete o m'
      
-- |Performs garbage collection on a variable.
ordvsCollect :: (IsVar id, Monad m) => [id] -> OrdVS id m HsExp
ordvsCollect sl
  = do
    assoc <- ordvsAssoc sl
    modify rmFocus
    return . HsEComp (HsEProj 2 2) $ assoc
  where
    rmFocus (VSSingle m) = bug m "Can't collect a single varset."
    rmFocus (VSFocus f m) = VSSingle . M.map (\x -> x - f) . M.filter (>= f) $ m
      
--------------------------------------------------------------------------------
-- ** Testing

runTestOrdVS :: (Monad m) => OrdVS String m a -> m a
runTestOrdVS f = runOrdVS ((mapM_ ordvsNewV $ words "a b c d") >> f)

runTestOrdVS__ :: (Monad m) => OrdVS String m a -> m (a, VS String)
runTestOrdVS__ f = runStateT ((mapM_ ordvsNewV $ words "a b c d") >> f) $ VSSingle M.empty


