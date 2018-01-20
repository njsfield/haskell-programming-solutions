{-# LANGUAGE InstanceSigs #-}

module Ch26.EitherT where

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT mea) =
    EitherT $ (fmap . fmap) f mea

instance Applicative m => Applicative (EitherT e m) where
  pure a = EitherT $ pure (Right a)
  (EitherT mf) <*> (EitherT ma) =
    EitherT $ fmap (<*>) mf <*> ma

instance Monad m => Monad (EitherT e m) where
  return = pure
  (EitherT mv) >>= f =
    EitherT $ do
      v <- mv
      case v of
        Left e -> return (Left e)
        Right a -> runEitherT (f a)

swapEither :: Either e a -> Either a e
swapEither (Left e) = Right e
swapEither (Right a) = Left a

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mea) =
  EitherT $ fmap swapEither mea

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT leftF rightF (EitherT mea) = do
  v <- mea
  case v of
    Left e -> leftF e
    Right a -> rightF a
