{-# LANGUAGE InstanceSigs #-}

module Moi where

newtype Moi s a = Moi { runMoi :: s -> (a, s)}

instance Functor (Moi s) where
        fmap :: (a -> b) -> Moi s a -> Moi s b
        fmap f (Moi g) = Moi g' where g' x = let (a, s) = g x in (f a, s)

instance Applicative (Moi s) where
        pure :: a -> Moi s a
        pure a = Moi $ \x -> (a, x)
        (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
        (Moi f) <*> (Moi g) = Moi h            where
                h s =
                        let (a , s2) = g s1
                            (f2, s1) = f s
                        in  (f2 a, s2)

                        -- This also type-checks, but it seems it is not 
                        -- the right implementation. Unsure why...
                        -- let (a , s1) = g s
                        --     (f2, s2) = f s1
                        -- in  (f2 a, s2)

instance Monad (Moi s) where
        return = pure
        (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
        (Moi f) >>= g = Moi h
                where h s = let (x, s2) = f s in runMoi (g x) s2

