data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where 
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure = undefined
  (<*>) = undefined

main = do
  let x = Cons 10 (Cons 5 Nil)
  print "Checking fmap"
  print x
  print $ fmap (+3) x
