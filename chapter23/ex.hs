module Main where

import           Moi

get :: Moi s s
get = Moi $ \s -> (s, s)

put :: s -> Moi s ()
put s = Moi $ const ((), s)

exec :: Moi s a -> s -> s
exec (Moi sa) = snd . sa

eval :: Moi s a -> s -> a
eval (Moi sa) = fst . sa

modify :: (s -> s) -> Moi s ()
modify f = Moi $ \s -> ((), f s)


main :: IO ()
main = do
        print $ runMoi get "curryIsAmaze"
        print $ runMoi (put "blah") "woot"
        print $ exec (put "wilma") "daphne"
        print $ exec get "scooby papu"
        print $ eval get "bunnicula"
        print $ runMoi (modify (+ 1)) 0
        print $ runMoi (modify (+ 1) >> modify (+ 1)) 0
