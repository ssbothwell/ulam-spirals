module Lib.Stream where


data Stream a = Stream a (Stream a)

instance Show a => Show (Stream a) where
  show stream = show (takeS 5 stream) <> "..."

instance Functor Stream where
  fmap f (Stream a rest) = Stream (f a) (fmap f rest)


mkStream :: (a -> a) -> a -> Stream a
mkStream f a = Stream a (mkStream f (f a))

takeS :: Int -> Stream a -> [a]
takeS 0 (Stream a as) = []
takeS i (Stream a as) = a : takeS (i - 1) as

(<->) :: [a] -> Stream a -> Stream a
(<->) [] stream = stream
(<->) (viewEnd -> (xs, [x])) stream = xs <-> Stream x stream
