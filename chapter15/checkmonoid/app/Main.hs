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

-- Combine

newtype Comp a = Comp {unComp :: (a -> a)}

instance Semigroup (Comp a) where
    f <> g = Comp $ (unComp g).(unComp f)
    
-- Not sure how to test this...
-- 
--

-- TESTING MONOID

monoidLeftIdentity :: (Eq m, Monoid m)
                   => m
		   -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m)
                   => m
		   -> Bool
monoidRightIdentity a = (a <> mempty) == a

-- Three

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three xx yy zz) <> (Three x y z) = Three (xx<>x) (yy<>y) (zz<>z) 

instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
  mempty = Three mempty mempty mempty
  mappend = (<>)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        x <- arbitrary
	y <- arbitrary
	z <- arbitrary
        return $ Three x y z

type ThreeAssoc a b c =
    (Three a b c) -> (Three a b c) -> (Three a b c) -> Bool

-- Mem

newtype Mem s a = Mem {runMem :: s -> (a,s)}

instance Semigroup a => Semigroup (Mem s a) where
  mem1<>mem2 = Mem fn where
    fn s = 
      let 
        (a1, s1) = (runMem mem1) s
        (a2, s2) = (runMem mem2) s1
      in
	(a1<>a2, s2)

instance Monoid a => Monoid (Mem s a) where
   mempty = Mem fempty where
     fempty s = (mempty, s)
   mappend = (<>)

f' = Mem $ \s -> ("hi", s + 1)


main :: IO()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  
  quickCheck (semigroupAssoc :: IdentityAssoc [Int])
  
  putStrLn $ show $ (Two [1] (Sum 2)) <> (Two [1] (Sum 2))
  quickCheck (semigroupAssoc :: TwoAssoc [Int] (Sum Int))

  putStrLn $ show $ (BoolConj True) <> (BoolConj False)
  quickCheck (semigroupAssoc :: BoolConjAssoc)

  quickCheck (semigroupAssoc :: OrAssoc Int Int)

  quickCheck (semigroupAssoc :: ThreeAssoc [Int] [Int] [Int]) 
  quickCheck (monoidRightIdentity :: (Three [Int] [Int] [Int]) -> Bool)
  quickCheck (monoidLeftIdentity :: (Three [Int] [Int] [Int]) -> Bool)

  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print "Mem sanity check"
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0


