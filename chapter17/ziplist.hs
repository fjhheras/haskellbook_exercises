import Control.Applicative
import Data.Monoid
import Test.QuickCheck 
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Some unfortunate orphan instances follow. Try to avoid these
-- in code you're going to keep or release.

instance Semigroup a => Semigroup (ZipList a) where
  (<>) = liftA2 (<>) 

instance Monoid a => Monoid (ZipList a) where
  mappend = liftA2 mappend --(<>)
  mempty  = pure mempty 
  --mempty = ZipList []

main :: IO ()
main = do
  quickBatch $ monoid (ZipList [1 :: Sum Int])
  -- everything runs but mconcat: runs forever
