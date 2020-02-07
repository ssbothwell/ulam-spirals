module Spiral where

import Data.List
import qualified Data.Map.Strict as M

-- https://en.wikipedia.org/wiki/Square_lattice
-- https://en.wikipedia.org/wiki/Ulam_spiral

result :: M.Map (Int, Int) Int
result = M.fromList
  [ ((0, 0), 6), ((1, 0), 7), ((2, 0), 8)
  , ((0, 1), 5), ((1, 1), 0), ((2, 1), 1)
  , ((0, 2), 4), ((1, 2), 3), ((2, 2), 2)
  ]


grid :: Int -> [[(Int, Int)]]
grid n = unfoldr (\b -> if b == n then Nothing else Just (row b n, b + 1)) 0
  where
    row i m = unfoldr (\b -> if b == m then Nothing else Just ((b, i), b + 1)) 0

data Dir = N | S | E | W

genSpiral :: Int -> [(Int, Int)]
genSpiral n = f elems path [(0, 0)] E
  where
    elems = n^2
    path = replicate 2 =<< [1..]
    f 0 _ grid _ = grid
    f m (p:ps) grid@((x,y):gs) dir = case dir of
      N -> f (m-p) ps (reverse [(x, y-i) | i <- [1..p]] ++ grid) E
      S -> f (m-p) ps ([(x, y+i) | i <- [1..p]] ++ grid) W
      E -> f (m-p) ps ([(x+i, y) | i <- [1..p]] ++ grid) S 
      W -> f (m-p) ps (reverse [(x-i, y) | i <- [1..p]] ++ grid) N

shiftOrigin :: Int -> [(Int, Int)] -> [(Int, Int)]
shiftOrigin n gd = fmap (\(x, y) -> (x+x1, y+y1)) gd 
  where
    (x1, y1) = minimum gd

addIndex :: [(Int, Int)] -> [((Int, Int), Int)]
addIndex sp = zip sp [0..]

mkSpiral :: Int -> M.Map (Int, Int) Int
mkSpiral = M.fromList . addIndex . (shiftOrigin <*> genSpiral)

printSpiral' :: M.Map (Int, Int) Int -> [[(Int, Int)]] -> [[Int]]
printSpiral' mp = (fmap . fmap) ((M.!) mp)


main = print $ mkSpiral 3 -- $ printSpiral' (mkSpiral 3) (grid 3)

n3 :: [(Int, Int)]
n3 = 
  [ (-1,-1), (0,-1), (1,-1)
  , (-1,0), (0,0), (1,0)
  , (-1,1), (0,1), (1,1)
  ]
{-
n=3
6 7 8
5 0 1
4 3 2
-1,-1 0,-1 1,-1
-1,0  0,0  1,0
-1,1  0,1  1,1
n=5
20 21 22 23 24
19 06 07 08 09
18 05 00 01 10
17 04 03 02 11
16 15 14 13 12
n=6
42 43 44 45 46 47 48
41 20 21 22 23 24 25
40 19 06 07 08 09 26
39 18 05 00 01 10 27
38 17 04 03 02 11 28
37 16 15 14 13 12 29
36 35 34 33 32 31 30
n=3
(-1,-1) (0,-1) (1,-1)
(-1,0)  (0,0)  (1,0)
(-1,1)  (0,1)  (1,1)
n=4
(-2, -1) (-1,-1) (0,-1) (1,-1)
(-2, 0)  (-1,0)  (0,0)  (1,0)
(-2, 1)  (-1,1)  (0,1)  (1,1)
(-2, 2)  (-1,2)  (0,2)  (1,2)
n=5
(-2, -2) (-1,-2) (0,-2) (1,-2) (2, -2)
(-2, -1) (-1,-1) (0,-1) (1,-1) (2, -1)
(-2, 0)  (-1,0)  (0,0)  (1,0)  (2, 0)
(-2, 1)  (-1,1)  (0,1)  (1,1)  (2, 1)
(-2, 2)  (-1,2)  (0,2)  (1,2)  (2, 2)
-}
