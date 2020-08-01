data Constant a b = Constant b deriving (Eq, Show)

instance Foldable (Constant a) where
        foldMap _ (Constant _) = mempty


data Two a b = Two a b deriving (Eq, Show)

instance Foldable (Two a) where
        foldMap f (Two x y) = f y


data Three a b c = Three a b c deriving (Eq, Show)

instance Foldable (Three a b) where
        foldMap f (Three _ _ x) = f x


data Three' a b = Three' a b b deriving (Eq, Show)

instance Foldable (Three' a) where
        foldMap f (Three' _ x y) = (f x) <> (f y)


data Four a b = Four a b b b deriving (Eq, Show)

instance Foldable (Four a) where
        foldMap f (Four _ x y z) = (f x) <> (f y) <> (f z)

---

filterF
        :: (Applicative f, Foldable t, Monoid (f a))
        => (a -> Bool)
        -> t a
        -> f a

filterF f = foldMap to_monoid    where
        to_monoid x | f x       = pure x
                    | otherwise = mempty

