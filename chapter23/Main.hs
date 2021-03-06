
module Main where

import           Control.Applicative            ( liftA3 )
import           Control.Monad                  ( replicateM )
import           Control.Monad.Trans.State
import           System.Random

data Die = DieOne | DieTwo | DieThree | DieFour | DieFive | DieSix deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n = case n of
        1 -> DieOne
        2 -> DieTwo
        3 -> DieThree
        4 -> DieFour
        5 -> DieFive
        6 -> DieSix
        x -> error $ "intToDie got non 1-6 integer: " ++ show x


-- state :: Monad m => (s -> (a, s)) -> StateT s m a

rollDie :: State StdGen Die
rollDie = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes :: State StdGen (Die, Die, Die)
rollDieThreeTimes = liftA3 (,,) rollDie rollDie rollDie

-- replicateM :: Monad m => Int -> m a -> m [a]

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
    where
        go :: Int -> Int -> StdGen -> Int
        go sum count gen
                | sum >= 20
                = count
                | otherwise
                = let (die, nextGen) = randomR (1, 6) gen
                  in  go (sum + die) (count + 1) nextGen

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go n 0 0 g
    where
        go :: Int -> Int -> Int -> StdGen -> Int
        go threshold sum count gen
                | sum >= threshold
                = count
                | otherwise
                = let (die, nextGen) = randomR (1, 6) gen
                  in  go threshold (sum + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go n 0 (0, []) g
    where
        go :: Int -> Int -> (Int, [Die]) -> StdGen -> (Int, [Die])
        go threshold sum countLog gen
                | sum >= threshold
                = countLog
                | otherwise
                = let (die      , nextGen) = randomR (1, 6) gen
                      (pastCount, pastLog) = countLog
                  in  go threshold
                         (sum + die)
                         (pastCount + 1, intToDie die : pastLog)
                         nextGen
---

main :: IO ()
main = do
        print $ evalState rollDie (mkStdGen 1)
        print $ evalState rollDieThreeTimes (mkStdGen 1)
        print $ evalState (nDie 3) (mkStdGen 1)
        print $ rollsToGetTwenty (mkStdGen 0)
