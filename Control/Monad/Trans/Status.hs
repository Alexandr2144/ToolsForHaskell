module Control.Monad.Trans.Status where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Status
import Control.Monad


newtype StatusT e m a = StatusT { runStatusT :: m (Status e a) }

mapStatusT :: (m (Status e a) -> n (Status e' b)) -> StatusT e m a -> StatusT e' n b
mapStatusT f = StatusT . f . runStatusT


instance Monoid e => MonadTrans (StatusT e) where
    lift = StatusT . liftM Success

instance (StatusLog e, MonadIO m) => MonadIO (StatusT e m) where
    liftIO = lift . liftIO

instance Monad m => Functor (StatusT e m) where
    fmap f = mapStatusT (fmap (fmap f))

instance (Functor m, Monad m, Monoid e) => Applicative (StatusT e m) where
    pure = lift . return

    tf <*> ta = StatusT $ do
        mf <- runStatusT tf
        ma <- runStatusT ta
        return $ mf <*> ma

instance (StatusLog e, Monad m) => Monad (StatusT e m) where
    return = lift . return
    
    ta >>= f = StatusT $ do
        ma <- runStatusT ta
        case ma of
            (Success a) ->  do
                mb <- runStatusT (f a)
                return (ma >> mb)
            (Warning e a) -> do
                mb <- runStatusT (f a)
                return (ma >> mb)
            (Exception e) -> return $ Exception e

    fail s = StatusT $ return (exception s)
