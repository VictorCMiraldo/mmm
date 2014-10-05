{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Pretty
-- Copyright   :  HASLAb team, University of Minho
-- License     :  MIT
--
-- Maintainer  :  Victor Miraldo <victor.cacciari@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
--Pretty printing class decl.
----------------------------------------------------------------------------
module MMM.Util.Pretty (
    Pretty(..)
  , module PP
  , t
  , sepBy
  , blocked
  , pprAssocLeftWith
  ) where

import Text.PrettyPrint as PP

-- |Pretty values
class Pretty a where
  pretty :: a -> Doc
  plvl   :: Int -> a -> Doc
  (<>+) :: Doc -> a -> Doc
  (+<>) :: a -> Doc -> Doc
  (<++) :: Doc -> a -> Doc
  (++>) :: a -> Doc -> Doc
  
  pretty = plvl 0
  plvl n = (nest n) . pretty
  d <>+ p = d <> (pretty p)
  p +<> d = (pretty p) <> d
  d <++ p = d <+> (pretty p)
  p ++> d = (pretty p) <+> d
  
instance (Pretty a) => Pretty (Maybe a) where
  pretty (Just a)   = pretty a
  pretty (Nothing)  = empty 
  
instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pretty (Left x)  = pretty x
  pretty (Right x) = pretty x
  
instance (Pretty a, Pretty b) => Pretty (a, b) where
  pretty (x, y) = pretty x $+$ t " X " $+$ pretty y
  
instance Pretty String where
  pretty = text
  
instance Pretty Doc where
  pretty = id
  
instance Pretty Int where
  pretty = int
  
instance Pretty Integer where
  pretty = t . show
  
-- Abreviations
t :: String -> Doc
t = text

tif :: String -> Bool -> Doc
tif s b = if b then (t s) else empty

sepBy :: (Pretty a) => [a] -> Doc ->  Doc
sepBy l d = hsep (punctuate d (map pretty l))

blocked :: (Pretty a) => a -> Doc
blocked a = t "{" $+$ plvl 1 a $+$ t "}"

pprAssocLeftWith :: (Pretty a) => (Doc -> a -> Doc) -> [a] -> Doc
pprAssocLeftWith f [] = empty
pprAssocLeftWith f (a:as) = foldl f (pretty a) as
