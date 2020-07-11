import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data List a = Nil | Cons a (List a) deriving (Eq, Show)

-- List

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

-- ZipList

-- Not using this take' because I am cheating a bit and I am 
-- using Haskell list instead of our own List
--
take' :: Int -> List a -> List a
take' _ Nil         = Nil
take' 0 xs          = Nil
take' n (Cons x xs) = Cons x $ take' (n - 1) xs

newtype ZipList' a = ZipList' [a] deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
        xs =-= ys = xs' `eq` ys'
            where
                xs' = let (ZipList' l) = xs in take 3000 l
                ys' = let (ZipList' l) = ys in take 3000 l

instance Functor ZipList' where
        fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Semigroup (ZipList' a) where
        (<>) (ZipList' xs) (ZipList' ys) = ZipList' (xs <> ys)

instance Monoid (ZipList' a) where
        mempty = ZipList' []

instance Applicative ZipList' where
        pure z = ZipList' $ repeat z
        --pure z = ZipList' [z]
        (<*>) _             (ZipList' []) = ZipList' []
        (<*>) (ZipList' []) _             = ZipList' []
        (<*>) (ZipList' (x : xs)) (ZipList' (x' : xs')) =
                ZipList' [x x'] <> (ZipList' xs <*> ZipList' xs')

instance (Arbitrary a) => Arbitrary (ZipList' a) where
        arbitrary = ZipList' <$> arbitrary

main = do
        let v = Cons 1 (Cons 2 Nil)
        let f = Cons (+ 1) (Cons (* 2) Nil)
        print "Checking fmap"
        print v
        print $ fmap (+ 3) v
        print "Checking f <*> v"
        print $ f <*> v
        print "---- ZipList ----"
        let zl' = ZipList'
        let z   = zl' [(+ 9), (* 2), (+ 8)]
        let z'  = zl' [1 .. 3]
        print $ z <*> z' -- [10, 4, 11]
        let z'' = zl' (repeat 1)
        print $ z <*> z'' --[10, 2, 9]
        quickBatch $ monoid (ZipList' [1 :: Int])
        quickBatch $ applicative (undefined :: ZipList' (Int, Float, String))
