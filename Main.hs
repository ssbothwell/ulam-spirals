module Main where
import Lib
import Data.Semigroup
import Data.List

main = print "hi"

data Stream a = Stream a (Stream a)
  deriving Functor

takeS :: Int -> Stream a -> [a]
takeS 0 (Stream a as) = []
takeS i (Stream a as) = a : takeS (i - 1) as

mkStream :: (a -> a) -> a -> Stream a
mkStream f a = Stream a (mkStream f (f a))

instance Show a => Show (Stream a) where
  show stream = show (takeS 5 stream) <> "..."

type Corner a = Vect ('S ('S 'Z)) (Sum a)

mkCorner :: (a, a) -> Corner a
mkCorner (a, b) = Sum a :. Sum b :. Nil

data Dir a = U (Sum a) | D (Sum a) | L (Sum a) | R (Sum a)
  deriving Show

dirToCorner :: Num a => Dir a -> Corner a
dirToCorner (U (Sum a)) = mkCorner (0, a)
dirToCorner (D (Sum a)) = mkCorner (0, negate a)
dirToCorner (L (Sum a)) = mkCorner (negate a, 0)
dirToCorner (R (Sum a)) = mkCorner (a, 0)

cornerToDir :: (Ord a, Num a) => Corner a -> Dir a
cornerToDir (viewCorner -> (x,  Sum 0))
  | x > Sum 0 = R x
  | otherwise = L $ negate x
cornerToDir (viewCorner -> (Sum 0, y))
  | y > Sum 0 = U y
  | otherwise = D $ negate y

nextDiff :: Num a => Dir a -> Dir a
nextDiff (U a) = L $ a <> pure 1
nextDiff (D a) = R $ a <> pure 1
nextDiff (L a) = D a
nextDiff (R a) = U a

nextCorner :: (Corner Int, Corner Int) -> (Corner Int, Corner Int)
nextCorner (f, s) =
  let diff = s `subV` f :: Corner Int
      x = (dirToCorner . nextDiff . cornerToDir) diff
  in (s, s <> x)

first = mkCorner (0, 0)
second = mkCorner (1, 0)

zero :: (Corner Int, Corner Int)
zero = (first, second)

tupleStream :: Stream (Vect ('S ('S 'Z)) Int)
tupleStream = (fmap getSum) . fst <$> mkStream nextCorner zero

viewEnd :: [a] -> ([a], [a])
viewEnd xs = splitAt (length xs - 1) xs

(<->) :: [a] -> Stream a -> Stream a
(<->) [] stream = stream
(<->) (viewEnd -> (xs, [x])) stream = xs <-> Stream x stream

func :: Stream [a] -> Stream a
func (Stream xs stream) = xs <-> func stream

corners :: [Corner Int]
corners = fmap mkCorner $
  [ ( 0,  0)
  , ( 1,  0)
  , ( 1,  1)
  , (-1,  1)
  , (-1, -1)
  , ( 2, -1)
  , ( 2,  2)
  , (-2,  2)
  ]

{-
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
 -}
