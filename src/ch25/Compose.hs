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
  ((.) fmap fmap) :  ((f a -> f b) -> (t (f a) -> t (f b))
                  -> (a -> b) -> (f a -> f b)
                  -> (a -> b) -> (t (f a) -> t (f b))
-}

instance (Functor f, Functor g) =>
          Functor (Compose f g) where
  fmap f (Compose fga) =
    Compose $ (fmap . fmap) f fga

{- Applicative
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

{- Foldable
   foldMap : (a -> m) -> g a -> m
   f : (a -> m)
   a : (f (g a))
   foldMap f : g a -> m
   foldMap (foldMap f) : f (g a) -> m
-}


instance (Foldable f, Foldable g) =>
          Foldable (Compose f g) where
  foldMap f (Compose a) = foldMap (foldMap f) a


{- Traversable
   traverse : (a -> f b) -> g a -> f (g b)
   f : (a -> f b)
   a : (f (g a))
   traverse f : g a -> f (g b)
   traverse (traverse f) : f (g a) -> h (f (g b))
-}
instance (Traversable f, Traversable g) =>
          Traversable (Compose f g) where
  traverse f (Compose a) = (Compose <$> traverse (traverse f)) a
