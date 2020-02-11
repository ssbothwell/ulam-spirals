module Main where

import Lib
import Control.Monad
import Control.Comonad
import Data.Semigroup
import Data.List

-- https://en.wikipedia.org/wiki/Square_lattice
-- https://en.wikipedia.org/wiki/Ulam_spiral

main = print "hi"

type Corner a = Vect ('S ('S 'Z)) (Sum a)

mkCorner :: (a, a) -> Corner a
mkCorner = (fmap Sum) . mkVector2

data Dir a = U (Sum a) | D (Sum a) | L (Sum a) | R (Sum a)
  deriving Show

dirToCorner :: Num a => Dir a -> Corner a
dirToCorner (U (Sum a)) = mkCorner (0, a)
dirToCorner (D (Sum a)) = mkCorner (0, negate a)
dirToCorner (L (Sum a)) = mkCorner (negate a, 0)
dirToCorner (R (Sum a)) = mkCorner (a, 0)

cornerToDir :: (Ord a, Num a) => Corner a -> Dir a
cornerToDir (viewTuple -> (x,  Sum 0))
  | x > Sum 0 = R x
  | otherwise = L $ negate x
cornerToDir (viewTuple -> (Sum 0, y))
  | y > Sum 0 = U y
  | otherwise = D $ negate y

nextDiff :: Num a => Dir a -> Dir a
nextDiff (U a) = L $ a <> pure 1
nextDiff (D a) = R $ a <> pure 1
nextDiff (L a) = D a
nextDiff (R a) = U a

nextCorner :: (Corner Int, Corner Int) -> (Corner Int, Corner Int)
nextCorner (f, s) =
  let diff = s .- f :: Corner Int
      x = (dirToCorner . nextDiff . cornerToDir) diff
  in (s, s <> x)

---

  -- Generate a list of vectors in a line between two Vects on a line
fromTo :: (Semigroup (Vect n a), Num a, Eq a) => (Vect n a, Vect n a) -> [Vect n a]
fromTo (v1, v2) =
  case v1 .- v2 of
    x :. 0 :. Nil -> undefined
    0 :. y :. Nil -> undefined
    otherwise -> error "Transform was not on a straight line"

------------
--- Main ---
------------

first = mkCorner (0, 0)
second = mkCorner (1, 0)

zero :: (Corner Int, Corner Int)
zero = (first, second)

tupleStream :: Stream (Vect ('S ('S 'Z)) Int)
tupleStream = (fmap getSum) . fst <$> mkStream nextCorner zero

-- Flatten a Stream of lists into a stream
func :: Stream [a] -> Stream a
func (Stream xs stream) = xs <-> func stream

tempStream :: Stream (Corner Int)
tempStream = func $ fromTo <$> mkStream nextCorner zero
