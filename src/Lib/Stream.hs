module Lib.Stream where


data Stream a = Stream a (Stream a)

instance Show a => Show (Stream a) where
  show stream = show (takeS 5 stream) <> "..."

instance Functor Stream where
  fmap f (Stream a rest) = Stream (f a) (fmap f rest)

instance Foldable Stream where
  foldMap f (Stream a rest) = f a <> foldMap f rest

mkStream :: (a -> a) -> a -> Stream a
mkStream f a = Stream a (mkStream f (f a))

takeS :: Int -> Stream a -> [a]
takeS 0 _ = []
takeS i (Stream a as) = a : takeS (i - 1) as

-- View the body and last element of a list
viewEnd :: [a] -> ([a], [a])
viewEnd xs = splitAt (length xs - 1) xs
