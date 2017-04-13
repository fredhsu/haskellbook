import           Test.QuickCheck
import           Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f =
    fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a->b)
    -> (b -> c)
    -> f a
    -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap(g . f) x)

functorCompose' :: (Eq (f c), Functor f) => f a
    ->Fun a b
    ->Fun b c
    -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
    (fmap (g . f) x)==(fmap g . fmap f $ x)

newtype Identity a = Identity a
    deriving (Eq)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

data Pair a = Pair a a
    deriving (Eq)

instance Functor Pair where
     fmap f (Pair a b) = Pair (f a) (f b)

incIfRight :: Num a => Either e a -> Either e a
incIfRight (Right n) = Right $ n + 1
incIfRight (Left e) = Left e

showIfRight :: Show a => Either e a -> Either e String
showIfRight (Right s) = Right $ show s
showIfRight (Left e) = Left e

incEither :: Num a => Either e a -> Either e a
incEither m = fmap (+1) m

showEither :: Show a => Either e a -> Either e String
showEither s = fmap show s

incEither' :: Num a => Either e a -> Either e a
incEither' = fmap (+1)

showEither' :: Show a => Either e a -> Either e String
showEither' = fmap show

liftedInc :: (Functor f, Num b) => f b -> f b
liftedInc = fmap (+1)

liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow = fmap show
