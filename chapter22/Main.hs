module Main where

import Data.Char

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

main :: IO ()
main = do
  print $ tuppledM "Cthulhu"
  print $ tuppledMdo "Cthulhu"
  print $ tuppledA "Cthulhu"
