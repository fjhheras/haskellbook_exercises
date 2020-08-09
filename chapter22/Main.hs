{-# LANGUAGE InstanceSigs #-}
module Main where

import Data.Char

newtype Reader r a =
  Reader { runReader :: r -> a }

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev 

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tuppledA :: [Char] -> ([Char], [Char])
tuppledA = (,) <$> cap <*> rev

tuppledMdo :: [Char] -> ([Char], [Char])
tuppledMdo = do
  x <- cap
  y <- rev
  return (x, y)

tuppledM :: [Char] -> ([Char], [Char])
tuppledM = cap >>= (\x -> rev >>= (\y -> return (x, y))) 

--

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f x y = f <$> x <*> y 

asks :: (r -> a) -> Reader r a
asks f = Reader f

--
instance Functor (Reader r) where
  fmap f (Reader g) = Reader (f . g)

instance Applicative (Reader r) where
  pure :: a -> Reader r a -- This would not work without InstanceSigs
  pure a = Reader $ const a
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r) 

instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (>>=) (Reader f) g = Reader $ \r -> runReader (g (f r)) r




main :: IO ()
main = do
  print $ tuppledM "Cthulhu"
  print $ tuppledMdo "Cthulhu"
  print $ tuppledA "Cthulhu"
