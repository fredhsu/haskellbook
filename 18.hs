import Control.Monad(join)
bind :: Monad m => (a -> m b) -> m a -> m b
bind x y = join $ fmap x y
