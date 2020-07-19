module Main where

import Data.Monoid (Sum(..), Product(..), Any(..))
import Control.Applicative (liftA2)

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = getAny . foldMap (Any . (==) x )

maybeMin :: (Ord a ) => a -> Maybe a -> Maybe a
maybeMin x = fmap (min x)

withFirst :: (a -> a -> a) -> (a -> Maybe a -> Maybe a)
withFirst f x Nothing = Just x
withFirst f x (Just y) = Just (f x y)

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr (withFirst min) Nothing

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr (withFirst max) Nothing

null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> False) True

length' :: (Foldable t) => t a -> Int
length' = getSum . foldMap (const (Sum 1))

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) [] 

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' = undefined

main :: IO ()
main = do
  print $ sum' [2, 3, 4]
  print $ elem' 2 [2, 3, 4]
  print $ elem' 0 [2, 3, 4]
  print $ minimum' [3, 4, 5]
  print $ minimum' ([] :: [String])
  print $ maximum' [3, 4, 5]
  print $ maximum' ([] :: [Int])
  print $ length' []
