data Validation e a = Failure e | Success a deriving (Eq, Show)

data Errors = StackOverflow | DividedByZero | BudgieChewedWires deriving (Eq, Show)

instance Functor (Validation e) where
        fmap f (Failure x) = Failure x
        fmap f (Success x) = Success (f x)

instance Monoid e => Applicative (Validation e) where
        pure = Success
        (<*>) (Success x) (Success x') = Success (x x')
        (<*>) (Failure e) (Success x ) = Failure e
        (<*>) (Success x) (Failure e ) = Failure e
        (<*>) (Failure e) (Failure e') = Failure (e <> e')

main = do
        print (Success (+ 1) <*> Success 1 :: Validation [Errors] Int)
        print
                (Failure [StackOverflow] <*> Success (+ 1) :: Validation
                          [Errors]
                          Int
                )
        print
                (Failure [StackOverflow] <*> Failure [BudgieChewedWires] :: Validation
                          [Errors]
                          Int
                )

