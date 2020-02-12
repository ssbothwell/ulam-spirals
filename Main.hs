module Main where

import Data.Foldable
import Data.List
import qualified Data.Map.Strict as M

-- https://en.wikipedia.org/wiki/Square_lattice
-- https://en.wikipedia.org/wiki/Ulam_spiral

data Dir a = U a | D a | L a | R a
  deriving Show

infixl 6 .-
(.-) :: Num a => (a, a) -> (a, a) -> (a, a)
(.-) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

infixl 6 .+
(.+) :: Num a => (a, a) -> (a, a) -> (a, a)
(.+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

dirToCorner :: Num a => Dir a -> (a, a)
dirToCorner = \case
  (U a) -> (0, a)
  (D a) -> (0, negate a)
  (L a) -> (negate a, 0)
  (R a) -> (a, 0)

cornerToDir :: (Ord a, Num a) => (a, a) -> Dir a
cornerToDir (x,  0)
  | x > 0 = R x
  | otherwise = L $ negate x
cornerToDir (0, y)
  | y > 0 = U y
  | otherwise = D $ negate y

nextDiff :: Num a => Dir a -> Dir a
nextDiff = \case
  (U a) -> L (a + 1)
  (D a) -> R (a + 1)
  (L a) -> D a
  (R a) -> U a

nextCorner :: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))
nextCorner (f, s) =
  let diff = s .- f
      newDiff = (dirToCorner . nextDiff . cornerToDir) diff
  in (s, s .+ newDiff)

from :: Int -> Int -> [Int]
from x y | x < y     = [x .. y]
         | otherwise = [x, x-1 .. y]

fromTo :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
fromTo (v1, v2) =
  case v1 .- v2 of
    (x, 0) -> [(x', snd v1) | x' <- from (fst v1) (fst v2)]
    (0, y) -> [(fst v2, y') | y' <- from (snd v1) (snd v2)]
    otherwise -> error "Transform was not on a straight line"

------------------
--- Gen Spiral ---
------------------

data Stream a = Stream a (Stream a)

instance Functor Stream where
  fmap f (Stream a rest) = Stream (f a) (fmap f rest)

instance Foldable Stream where
  foldMap f (Stream a rest) = f a <> foldMap f rest

mkStream :: (a -> a) -> a -> Stream a
mkStream f a = Stream a (mkStream f (f a))

viewEnd :: [a] -> ([a], [a])
viewEnd xs = splitAt (length xs - 1) xs

chop :: Stream [a] -> Stream [a]
chop (Stream (viewEnd -> (xs, _)) rest) = Stream xs (chop rest)

genStream :: Stream [(Int, Int)]
genStream = chop $ fromTo <$> mkStream nextCorner ((0, 0), (1, 0))

genList :: Int -> [(Int, Int)]
genList i = take i . fold $ genStream


-----------------------
--- Pretty Printing ---
-----------------------

shiftOrigin :: [(Int, Int)] -> [(Int, Int)]
shiftOrigin gd = fmap (.- (minimum gd)) gd

addIndex :: [(Int, Int)] -> [((Int, Int), Int)]
addIndex sp = zip sp [0..]

mkMap :: [(Int, Int)] -> M.Map (Int, Int) Int
mkMap = M.fromList . addIndex . shiftOrigin 

grid :: Int -> [[(Int, Int)]]
grid n = unfoldr (\b -> if b == n then Nothing else Just (row b n, b + 1)) 0
  where
    row i m = unfoldr (\b -> if b == m then Nothing else Just ((b, i), b + 1)) 0

genSpiral :: Int -> [[Int]]
genSpiral n =
  let mp = mkMap $ genList (n^2)
      grid' = grid n
  in (fmap . fmap) ((M.!) mp) grid'

printSpiral :: Int -> IO ()
printSpiral n = traverse_ print (genSpiral n)

------------
--- Main ---
------------

main :: IO ()
main = printSpiral 10
