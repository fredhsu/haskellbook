data Two a b =
    Two a b
    deriving (Eq, Show)

data Or a b =
    First a
    | Second b

instance Functor (Two a) where fmap = undefined
instance Functor (Or a) where fmap = undefined
