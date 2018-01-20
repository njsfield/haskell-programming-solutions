module Ch25.BiMap where

import           Prelude hiding (Either, Left, Right)

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

-- 1)

data Deux a b = Deux a b

instance Bifunctor Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)
  first f (Deux a b) = Deux (f a) b
  second f (Deux a b) = Deux a (f b)

-- 2)

data Const a b = Const a

instance Bifunctor Const where
  bimap f g (Const a) = Const (f a)
  first f (Const a) = Const (f a)

-- 3)

data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)
  first f (Drei a b c) = Drei a (f b) c
  second f (Drei a b c) = Drei a b (f c)

-- 4)

data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei a b) = SuperDrei a (f b)
  first f = bimap f id
  second _ (SuperDrei a b) = SuperDrei a b

-- 5)

data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei a) = SemiDrei a
  first _ (SemiDrei a) = SemiDrei a
  second _ (SemiDrei a) = SemiDrei a

-- 6)

data Quadriceps a b c d =
  Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)
  first f (Quadzzz a b c d) = Quadzzz a b (f c) d
  second f (Quadzzz a b c d) = Quadzzz a b c (f d)

-- 7)

data Either a b =
    Left a
  | Right b

instance Bifunctor Either where
  bimap f _ (Left a)  = Left (f a)
  bimap _ g (Right b) = Right (g b)
  first f (Left a)  = Left (f a)
  first _ (Right b) = Right b
  second _ (Left a)  = Left a
  second f (Right b) = Right (f b)
