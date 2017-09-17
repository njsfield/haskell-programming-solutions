{-# LANGUAGE FlexibleInstances #-}

module Ch16.ChapterExercises where

import           GHC.Arr

{-
 - Determine if Functor can be written for these datatypes
 -}

-- 1
data Bool' = False | True

-- Cannot implement, as Bool has kind *, a valid functor needs at least * -> *

-- 2
data BoolAndSomethingElse a =
  False' a | True' a

instance Functor BoolAndSomethingElse where
  fmap f (False' a) = False' (f a)
  fmap f (True' a)  = False' (f a)

-- 3
data BoolAndMaybeSomethingElse a =
  Falsish | Truish a

instance Functor BoolAndMaybeSomethingElse where
  fmap _ Falsish    = Falsish
  fmap f (Truish a) = Truish (f a)

-- 4
newtype Mu f =
  InF { outF :: f (Mu f) }

-- f has kind * -> *, hard to implement!

-- 5
data D = D (Array Word Word) Int Int

-- Cannot implement, as D has kind *

{-
 - Rearrange the arguments so the Functor instance works
 -}

-- 1
data Sum a b =
    First b
  | Second a

instance Functor (Sum e) where
  fmap f (First a)  = First (f a)
  fmap f (Second b) = Second b

-- 2
data Company a b c =
    DeepBlue a b
  | Something c
instance Functor (Company e e') where
  fmap f (Something b)  = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

data More a b =
    L b a b
  | R a b a
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'


{-
 - Write Functor instances
 -}

-- 1

data Quant a b =
    Finance
  | Desk a
  | Bloor b

instance Functor (Quant a) where
  fmap _ Finance   = Finance
  fmap _ (Desk a)  = Desk a
  fmap f (Bloor b) = Bloor ( f b)

-- 2

data K a b =
  K a

instance Functor (K a) where
  fmap _ (K a) = K a


-- 3

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

newtype K' a b =
  K' a

instance Functor (Flip K' a) where
  fmap f (Flip (K' a)) = Flip $ K' (f a)

-- 4

data EvilGoateeConst a b =
  GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

-- 5
data LiftItOut f a =
  LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

-- 6
data Parappa f g a =
  DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

-- 7
data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)

instance (Functor f, Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa ga) = IgnoringSomething fa (fmap f ga)

-- 8
data Notorius g o a t =
  Notorius (g o) (g a) (g t)

instance (Functor g) => Functor (Notorius g o a) where
  fmap f (Notorius go ga gt) = Notorius go ga (fmap f gt)

-- 9
data List' a =
    Nil
  | Cons a (List' a)

instance Functor List' where
  fmap _ Nil        = Nil
  fmap f (Cons a b) = Cons (f a) (fmap f b)

-- 10
data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat               = NoGoat
  fmap f (OneGoat a)          = OneGoat (f a)
  fmap f (MoreGoats ga gb gc) = MoreGoats (f <$> ga) (f <$> gb) (f <$> gc)

-- 11
data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt        = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read sa)   = Read (fmap f sa)
