{-# LANGUAGE InstanceSigs #-}

module Ch25.Compose where

-- F & G represent type constructors
-- not functions

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

{- Functor (fmap)
  (.)  : (x -> y) -> (v -> x) -> (v -> y)
  fmap : (a -> b) -> (f a -> f b)
  ((.) fmap famp) :  ((f a -> f b) -> (t (f a) -> t (f b))
                  -> (a -> b) -> (f a -> f b)
                  -> (a -> b) -> (t (f a) -> t (f b))
-}

instance (Functor f, Functor g) =>
          Functor (Compose f g) where
  fmap f (Compose fga) =
    Compose $ (fmap . fmap) f fga

{- Aapplicative
  (<*>) : t (x -> y) -> t x -> t y
  fmap (<*>) : f (t (x -> y)) -> f (t a -> t b)
  fmap (<*>) fgab : f (t a -> t b)
  (fmap (<*>) fgab) <*> fga : f g b
-}

instance (Applicative f, Applicative g) =>
          Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure a = Compose $ pure (pure a)

  (<*>) :: Compose f g (a -> b)
        -> Compose f g a
        -> Compose f g b
  (Compose f) <*> (Compose a) =
    Compose $ (fmap (<*>) f) <*> a

instance (Foldable f, Foldable g) =>
          Foldable (Compose f g) where
  foldMap f (Compose a) = foldMap (foldMap f) a

instance (Traversable f, Traversable g) =>
          Traversable (Compose f g) where
  traverse f (Compose a) = Compose <$> traverse (traverse f) a
