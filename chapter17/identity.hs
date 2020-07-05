module Main where

import Data.Monoid
-- I do not understand why import Data.Monoid (Sum, ) does not work
-- It does not complain in the import, but then it says:
-- Data constructor not in scope: Sum :: t0 -> a

-- identity as applicative
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity x) = Identity (f x)

-- constant as applicative

--type C = Constant
newtype Constant a b = Constant { getConstant :: a} deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant f) = Constant f

instance Monoid a => Applicative (Constant a) where
  pure x = Constant mempty 
  (<*>) (Constant f) (Constant g) = Constant (mappend f g)


main :: IO ()
main = do
  print "----Identity----"
  let xs = [1, 2, 3]
  let xs' = [9, 9, 9]
  print $ const <$> xs <*> xs'
  print $ const <$> Identity xs <*> Identity xs'
  print "----Constant----"
  let f = Constant (Sum 1)
  let g = Constant (Sum 2)
  print $ f <*> g
  print (pure 3 :: Constant (Sum Int) Int)
  print (pure 1 :: Constant String Int)
  print "----Maybe-----"
  print $ Just (+3) <*> Just 3
