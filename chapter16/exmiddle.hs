import FunctorLaws
import Test.QuickCheck
import Test.QuickCheck.Function

newtype Identity a = Identity a
instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

data Pair a = Pair a a 
instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

data Two a b = Two a b
instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

-- Skipped Three Three'

data Four a b c d = Four a b c d
instance Functor (Four a b c) where
  fmap f (Four x y z w) = Four x y z (f w)

data Four' a = Four' a a a a deriving (Eq, Show)
instance Functor Four' where
  fmap f (Four' x y z w) = Four' (f x) (f y) (f z) (f w)

-- This was not an exercise but I wanted to quickCheck the Functors
-- We are lifting endomorphisms in Int to endomrphisms in Four' Int
type IntToInt = Fun Int Int
fc' :: Int -> Int -> Int -> Int -> IntToInt -> IntToInt -> Bool
fc' x y z w = functorCompose' (Four' x y z w)

-- Possibly and Sum

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers x) = Yeppers (f x)

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x) = First x
  fmap f (Second x) = Second (f x)

main :: IO ()
main = do
  verboseCheck fc' 
  putStrLn "All passed!"
