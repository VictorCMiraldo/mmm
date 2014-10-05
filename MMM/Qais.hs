-----------------------------------------------------------------------------
-- |
-- Module      :  Qais
-- Copyright   :  HASLAb team, University of Minho
-- License     :  MIT
--
-- Maintainer  :  Victor Miraldo <victor.cacciari@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
--Probabilistic extension of 'MMM.Core.CombComb'.
----------------------------------------------------------------------------
module MMM.Qais where

import MMM.Core.FuncComb
import MMM.Core.Probability

import Data.List
import System.Cmd
import GHC.IO.Exception


-- choice between sharp f and g
schoice :: ProbRep -> (t -> a) -> (t -> a) -> t -> Dist a
schoice p f g x = choose p (f x) (g x)

-- choice between probabilistic f and g
choice :: ProbRep -> (t -> Dist a) -> (t -> Dist a) -> t -> Dist a
choice p f g x = cond (choose p True False) (f x) (g x)

-- Support of distribution:

support :: Eq a => Dist a -> [a]
support = nub . (map fst) . unD

--Size:

size :: Eq a => Dist a -> Int
size = length . support

--Compressing (for efficiency):

compress d = D [ (a,foldr (+) 0 (sel a (unD d))) | a <- support d] where
  sel :: Eq a => a -> [(a,b)] -> [b]
  sel a x = [ b | (a',b) <- x , a'==a ] 

-- pairing:
-- split
(f `kr` g) a = do { b <- f a ;
                    c <- g a ;
                    return (b,c)
                }
--fst
mfst d = do { (b,c) <- d ;
              return b
            }

--snd
msnd d = do { (b,c) <- d ;
             return c
            }

-- Monadic foldr

mfoldr :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
mfoldr f u [] = return u
mfoldr f u (a:x) = do { y <- mfoldr f u x ; f a y }

-- Monadic maps

mmap1 :: Monad m => (t1 -> [a] -> m [a]) -> (t -> t1) -> [t] -> m [a]
mmap1 cons _ [] = return []
mmap1 cons f (h:t) = do {
		        nt <- mmap1 cons f t;
		        cons (f h) nt;
		 }

mmap2 :: Monad m => (t -> m a) -> [t] -> m [a]
mmap2 _ [] = return []
mmap2 f (h:t) = do {
		    nt <- mmap2 f t;
		    nh <- f h;
		    return (nh:nt);
		 }

-- LaTeX interface (showing Dist graphically)

pdfit :: (Show a, Ord a) => Dist a -> IO GHC.IO.Exception.ExitCode
pdfit d = do
             dis2file d
             system "pdflatex _"
             system "open _.pdf"

dis2file :: (Show a, Ord a) => Dist a -> IO ()
dis2file = (writeFile "_.tex") . latexDist

latexDist :: (Show a, Ord a) => Dist a -> [Char]
latexDist = article . document . math . hist

article t= "\\documentclass{article}\n" ++ t

document d = "\\begin{document}\n" ++ d ++ "\\end{document}\n"

math m = "\\[\n" ++ m ++ "\\]\n"

hist d = "\\begin{array}{ll}\n" ++ (concat $ map histp x) ++ "\\end{array}\n" 
          where x = (sort . map (id >< (round.(100*))) . unD . compress) d 
                histp(n,p) = show n ++ " & \\rule{" ++ show p ++ "mm}{3pt}\\ " ++ show p ++"\\%\\\\\n"

