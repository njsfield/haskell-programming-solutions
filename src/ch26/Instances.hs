module Ch26.Instances where

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class

class Monad m => MonadIO m where
  liftIO :: IO a -> m a

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO i = lift . liftIO $ i

instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO i = lift . liftIO $ i

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO i = lift . liftIO $ i
