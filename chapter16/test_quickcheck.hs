import Test.QuickCheck
import FunctorLaws

f :: [Int] -> Bool
f x = functorIdentity x

c = functorCompose (+1) (+2)
li :: Maybe Int -> Bool
li x = c x

-- We are lifting Int -> Int morphisms to [Int] -> [Int]
type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool
fc' :: IntFC
fc' = functorCompose'

main :: IO()
main = do
  quickCheck f
  quickCheck li
  quickCheck fc'
