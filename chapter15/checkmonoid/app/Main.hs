module Main where

import Lib
import Control.Monad
import Data.Monoid
import Test.QuickCheck

semigroupAssoc :: (Eq m, Semigroup m)
            => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

-- Trivial

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial 

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool

-- Identity

data Identity a = Identity a deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a<>b) 

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = do
        x <- arbitrary
        return $ Identity x

type IdentityAssoc a =
    (Identity a) -> (Identity a) -> (Identity a) -> Bool

-- Two

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x y) <> (Two z w) = Two (x<>z) (y<>w) 

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        x <- arbitrary
	y <- arbitrary
        return $ Two x y

type TwoAssoc a b =
    (Two a b) -> (Two a b) -> (Two a b) -> Bool

-- I skip Three, Four
-- BoolConj

data BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj a) <> (BoolConj b) = BoolConj (a && b)

instance Arbitrary BoolConj where
  arbitrary = do
    x <- arbitrary 
    return (BoolConj x)

type BoolConjAssoc =
  BoolConj -> BoolConj -> BoolConj -> Bool

-- I skip BoolDisj
-- Or a b

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  (Snd a) <> _ = Snd a
  _ <> x = x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    elements [(Fst x), (Snd y)]

type OrAssoc a b =
  (Or a b) -> (Or a b) -> (Or a b) -> Bool




main :: IO()
main = do 
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc [Int])
  
  putStrLn $ show $ (Two [1] (Sum 2)) <> (Two [1] (Sum 2))
  quickCheck (semigroupAssoc :: TwoAssoc [Int] (Sum Int))

  putStrLn $ show $ (BoolConj True) <> (BoolConj False)
  quickCheck (semigroupAssoc :: BoolConjAssoc)

  quickCheck (semigroupAssoc :: OrAssoc Int Int)
