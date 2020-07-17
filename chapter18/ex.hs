import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes


data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
        fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
        pure _ = NopeDotJpg
        _ <*> _ = NopeDotJpg

instance Monad Nope where
        return = pure
        NopeDotJpg >>= _ = NopeDotJpg

instance Eq a => EqProp (Nope a) where
        (=-=) = eq

instance Arbitrary (Nope a) where
        arbitrary = return NopeDotJpg


-- Phhhhhh

data PEither b a = PLeft a | PRight b deriving (Eq, Show)

instance Functor (PEither b) where
        fmap f (PLeft  a) = PLeft (f a)
        fmap _ (PRight b) = PRight b

instance Applicative (PEither b) where
        pure = PLeft
        PRight x <*> _        = PRight x
        _        <*> PRight x = PRight x
        PLeft x  <*> PLeft  y = PLeft (x y)

instance Monad (PEither b) where
        return = pure
        PRight x >>= _ = PRight x
        PLeft  x >>= f = f x

instance (Eq a, Eq b) => EqProp (PEither b a) where
        (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (PEither b a) where
        arbitrary = oneof [PLeft <$> arbitrary, PRight <$> arbitrary]

-- Identity

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (Identity a) where
        arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
        (=-=) = eq

instance Functor Identity where
        fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
        pure = Identity
        (Identity f) <*> (Identity x) = Identity (f x)

instance Monad Identity where
        return = pure
        (Identity x) >>= f = f x

-- List

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
        fmap f Nil         = Nil
        fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Semigroup (List a) where
        (<>) Nil         x   = x
        (<>) x           Nil = x
        (<>) (Cons x xs) ys  = Cons x (xs <> ys)

instance Monoid (List a) where
        mempty = Nil

instance Applicative List where
        pure x = Cons x Nil
        (<*>) x           Nil = Nil
        (<*>) Nil         y   = Nil
        (<*>) (Cons x xs) ys  = (x <$> ys) <> (xs <*> ys)

instance Monad List where
        return = pure
        Nil         >>= f = Nil
        (Cons x xs) >>= f = f x <> (xs >>= f)

instance Arbitrary a => Arbitrary (List a) where
        arbitrary = frequency
                [(1, return Nil), (4, Cons <$> arbitrary <*> arbitrary)]

instance Eq a => EqProp (List a) where
        (=-=) = eq --maybe here I should check only the first N elements?

---

j :: Monad m => m (m a) -> m a
j x = x >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f x y = f <$> x <*> y

a :: Monad m => m a -> m (a -> b) -> m b
a x f = f <*> x

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] f = return []
meh (x:xs) f = fmap (:) (f x) <*> meh xs f

flipType :: Monad m => [m a] -> m [a]
flipType xs = meh xs id



main = do
        -- let     trigger :: Nope (Int, String, Int)
        -- let     trigger :: PEither Int (Int, String, Int)
        -- let     trigger :: Identity (Int, String, Int)
        let     trigger :: List (Int, String, Int)
                trigger = undefined
        verboseBatch $ functor trigger
        verboseBatch $ applicative trigger
        verboseBatch $ monad trigger

        print $ j [[1, 2], [], [3]]
        print $ j (Just (Just 1))
        print $ j (Just (Nothing :: Maybe Int))
        print $ j (Nothing :: Maybe (Maybe Int))



