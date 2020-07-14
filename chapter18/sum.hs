import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes


data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
        fmap _ (First  x) = First x
        fmap f (Second x) = Second (f x)

instance Applicative (Sum a) where
        pure = Second
        (<*>) (First x)  _          = First x
        (<*>) _          (First  x) = First x
        (<*>) (Second x) (Second y) = Second (x y)

instance Monad (Sum a) where
        return = pure
        (>>=) (First  x) f = First x
        (>>=) (Second x) f = f x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
        arbitrary = oneof [First <$> arbitrary, Second <$> arbitrary]

instance (Eq a, Eq b) => EqProp (Sum a b) where
        (=-=) = eq

main :: IO ()
main = do
        let     triggerSum :: (Sum Int) (Int, String, Int)
                triggerSum = undefined
        verboseBatch $ functor triggerSum
        verboseBatch $ applicative triggerSum
        verboseBatch $ monad triggerSum


