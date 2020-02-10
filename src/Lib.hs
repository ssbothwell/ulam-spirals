module Lib where

import Data.Monoid
import Data.Foldable
import Data.Kind

-------------
--- Types ---
-------------

data Nat = Z | S Nat
type Matrix n m a = Vect n (Vect m a)

infixr 5 :.
data Vect (n :: Nat) (a :: Type) where
  Nil :: Vect 'Z a
  (:.) :: a -> Vect n a -> Vect ('S n) a

viewCorner :: Vect ('S ('S 'Z)) a -> (a, a)
viewCorner (a :. b :. Nil) = (a, b)

instance Show a => Show (Vect n a) where
  show Nil = "()"
  show (a :. Nil) = "(" <> show a <> ")"
  show vect = "(" <> f vect <> ")"
    where f :: Show a => Vect n a -> String
          f (a :. Nil) = show a
          f (a :. as) = show a <> ", " <> f as

instance Semigroup (Vect 'Z a) where
  (<>) :: Vect 'Z a -> Vect 'Z a -> Vect 'Z a
  (<>) Nil Nil = Nil

instance (Semigroup (Vect n a), Semigroup a) => Semigroup (Vect ('S n) a) where
  (<>) :: Vect ('S n) a -> Vect ('S n) a -> Vect ('S n) a
  (<>) (a :. as) (b :. bs) = (a <> b) :. (as <> bs)

instance Monoid (Vect 'Z a) where
  mempty = Nil

instance (Monoid (Vect n a), Monoid a) => Monoid (Vect ('S n) a) where
  mempty = mempty :. mempty

instance Functor (Vect n) where
  fmap :: (a -> b) -> Vect n a -> Vect n b
  fmap _ Nil = Nil
  fmap f (a :. as) = (f a) :. (f <$> as)

instance Foldable (Vect n) where
  foldMap :: Monoid m => (a -> m) -> Vect n a -> m
  foldMap _ Nil = mempty
  foldMap f (a :. as) = f a <> foldMap f as

--mkVector :: [a] -> Vect n a
--mkVector [] = Nil
--mkVector (x:xs) = x :. mkVector xs

------------------
--- Operations ---
------------------

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

vZipWith :: (a -> b -> c) -> Vect n a -> Vect n b -> Vect n c
vZipWith _ Nil Nil = Nil
vZipWith f (a :. as) (b :. bs) = (f a b) :. (vZipWith f as bs)

scalarMult :: Num a => a -> Vect n a -> Vect n a
scalarMult a = fmap (* a)

vconcat :: Monoid a => Vect n a -> a
vconcat = fold

transform :: (Monoid (Vect m (Sum a)), Num a) => Matrix n m a -> Vect n a -> Vect m a
transform mat vect = getSum <$> (vconcat $ vZipWith ((<$$>) Sum . scalarMult) vect mat)

swapM :: Matrix ('S ('S 'Z)) ('S ('S 'Z)) Int
swapM = jhat :. ihat :. Nil

dilate :: Int -> Int -> Matrix ('S ('S 'Z)) ('S ('S 'Z)) Int
dilate x y = scalarMult x ihat :. scalarMult y jhat :. Nil

subV :: (Semigroup (Vect n a), Num a) => Vect n a -> Vect n a -> Vect n a
subV x y = x <> (scalarMult (negate 1) y)

----------------
--- Indexing ---
----------------

infixl 9 !!!
(!!!) :: Vect n a -> Int -> Maybe a
(!!!) Nil _       = Nothing
(!!!) (x :. _)    0 = Just x
(!!!) (_ :. rest) i = rest !!! (i - 1)


--------------------
--- Common Terms ---
--------------------

ihat :: Vect ('S ('S 'Z)) Int
ihat = 1 :. 0 :. Nil

jhat :: Vect ('S ('S 'Z)) Int
jhat = 0 :. 1 :. Nil

iden :: Matrix ('S ('S 'Z)) ('S ('S 'Z)) Int
iden = ihat :. jhat :. Nil


--------------------------------
--- Common Rotation Matrices ---
--------------------------------

ninetyCCW :: Num a => Matrix ('S ('S 'Z)) ('S ('S 'Z)) a
ninetyCCW = v1 :. v2 :. Nil
  where
    v1 :: Num a => Vect ('S ('S 'Z)) a
    v1 = 0 :. 1 :. Nil
    v2 :: Num a => Vect ('S ('S 'Z)) a
    v2 = negate 1 :. 0 :. Nil

oneeightyCCW :: Num a => Matrix ('S ('S 'Z)) ('S ('S 'Z)) a
oneeightyCCW = v1 :. v2 :. Nil
  where
    v1 :: Num a => Vect ('S ('S 'Z)) a
    v1 = negate 1 :. 0 :. Nil
    v2 :: Num a => Vect ('S ('S 'Z)) a
    v2 = 0 :. negate 1 :. Nil

twoseventyCCW :: Num a => Matrix ('S ('S 'Z)) ('S ('S 'Z)) a
twoseventyCCW = v1 :. v2 :. Nil
  where
    v1 :: Num a => Vect ('S ('S 'Z)) a
    v1 = 0 :. 1 :. Nil
    v2 :: Num a => Vect ('S ('S 'Z)) a
    v2 = negate 1 :. 0 :. Nil
