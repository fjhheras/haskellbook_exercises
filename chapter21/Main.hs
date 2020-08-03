{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes


newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
        fmap f (Identity x) = Identity (f x)

instance Foldable Identity where
        foldMap f (Identity x) = f x

instance Traversable Identity where
        traverse f (Identity x) = Identity <$> f x

instance Arbitrary a => Arbitrary (Identity a) where
        arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
        (=-=) = eq

---

newtype Constant a b = Constant { getConstant :: a} deriving (Eq, Ord, Show)

instance Functor (Constant a) where
        fmap f (Constant x) = Constant x

instance Foldable (Constant a) where
        foldMap _ _ = mempty

instance Traversable (Constant a) where
        traverse _ (Constant x) = Constant <$> pure x

instance Arbitrary a => Arbitrary (Constant a b) where
        arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where
        (=-=) = eq

---

data Optional a = Nada | Yep a

instance Functor Optional where
        fmap _ Nada    = Nada
        fmap f (Yep x) = Yep (f x)

instance Foldable Optional where
        foldMap _ Nada    = mempty
        foldMap f (Yep x) = f x

instance Traversable Optional where
        traverse f (Yep x) = Yep <$> f x

instance Arbitrary a => Arbitrary (Optional a) where
        arbitrary = oneof [pure Nada, Yep <$> arbitrary]

---

data List a = Nil | Cons a (List a) deriving (Eq, Ord, Show)

instance Functor List where
        fmap _ Nil         = Nil
        fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
        foldMap _ Nil         = mempty
        foldMap f (Cons x xs) = f x <> foldMap f xs

instance Traversable List where
        traverse _ Nil         = pure Nil
        traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

instance Arbitrary a => Arbitrary ( List a ) where
        arbitrary = oneof [pure Nil, Cons <$> arbitrary <*> arbitrary]

instance Eq a => EqProp (List a) where
        (=-=) = eq -- never stops for infinite lists

---

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
        fmap f (Three x y z) = Three x y (f z)

instance Foldable (Three a b) where
        foldMap f (Three _ _ x) = f x

instance Traversable (Three a b) where
        traverse f (Three x y z) = Three x y <$> f z

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
        arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
        (=-=) = eq

---

data Big a b = Big a b b deriving (Eq, Ord, Show)

instance Functor (Big a) where
        fmap f (Big x y z) = Big x (f y) (f z)

instance Foldable (Big a) where
        foldMap f (Big x y z) = f y <> f z

instance Traversable (Big a) where
        traverse f (Big x y z) = Big <$> pure x <*> f y <*> f z

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
        arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary


instance (Eq a, Eq b) => EqProp (Big a b) where
        (=-=) = eq

--

data S n a = S (n a) a deriving (Eq, Show)

-- Test for functor fail...
instance (Functor n) => Functor (S n) where
        fmap f (S na a) = S (fmap f na) (f a)

instance (Foldable n) => Foldable (S n) where
        foldMap f (S na a) = f a <> foldMap f na

-- test for traversable fail as well...
instance Traversable n => Traversable (S n) where
        traverse f (S na a) = S <$> (traverse f na) <*> f a

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
        arbitrary = S <$> arbitrary <*> arbitrary

instance (Applicative n, Testable (n Property),  EqProp a) => EqProp (S n a) where
        (S x y) =-= (S p q) = (property $ (=-=) <$> x <*> p) .&. (y =-= q)

---

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
        fmap _ Empty          = Empty
        fmap f (Leaf x      ) = Leaf (f x)
        fmap f (Node t1 x t2) = Node (f <$> t1) (f x) (f <$> t2)

instance Foldable Tree where
        foldMap _ Empty          = mempty
        foldMap f (Leaf x      ) = f x
        foldMap f (Node t1 x t2) = foldMap f t1 <> f x <> foldMap f t2

instance Traversable Tree where
        traverse _ Empty    = pure Empty
        traverse f (Leaf x) = Leaf <$> f x
        traverse f (Node t1 x t2) =
                Node <$> traverse f t1 <*> f x <*> traverse f t2

instance (Arbitrary a) => Arbitrary (Tree a) where
        arbitrary = oneof
                [ pure Empty
                , Leaf <$> arbitrary
                , Node <$> arbitrary <*> arbitrary <*> arbitrary
                ]

instance (Eq a) => EqProp (Tree a) where
        (=-=) = eq

type TI = Tree

main :: IO ()
main = do
        let     trigger :: TI (Int, Int, [Int])
                trigger = undefined
        quickBatch (functor trigger)
        quickBatch (traversable trigger)

