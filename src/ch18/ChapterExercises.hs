module Ch18.ChapterExercises where

import           Control.Applicative
import           Control.Monad            (ap, join, liftM, liftM2)
import           Data.Monoid              (Monoid, (<>))
import           Test.QuickCheck          (Arbitrary, arbitrary, elements,
                                           frequency)
import           Test.QuickCheck.Checkers (EqProp, eq, quickBatch, (=-=))
import           Test.QuickCheck.Classes  (applicative, functor, monad)

-- Monad Instances
-- 1)

data Nope a =
  NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap _ (NopeDotJpg) = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  return _ = NopeDotJpg
  (>>=) _ _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where (=-=) = eq

nopeTrigger :: Nope (String, String, String)
nopeTrigger = undefined

-- 2)
data PhhhbbtttEither b a =
    Left' a
  | Right' b
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap _ (Right' b) = (Right' b)
  fmap f (Left' a)  = Left' (f a)

instance Applicative (PhhhbbtttEither b) where
  pure a = Left' a
  (<*>) (Right' b) _        = Right' b
  (<*>) _ (Right' b)        = Right' b
  (<*>) (Left' f) (Left' a) = Left' (f a)

instance Monad (PhhhbbtttEither b) where
  return = pure
  (>>=) (Right' b) _ = Right' b
  (>>=) (Left' a) f  = f a

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [(Right' b), (Left' a)]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where (=-=) = eq

phhhbbtttEitherTrigger :: PhhhbbtttEither (String, String, String) (String, String, String)
phhhbbtttEitherTrigger = undefined

--3)
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure a = Identity a
  (<*>) (Identity f) (Identity a) = Identity (f a)

instance Monad Identity where
  return = pure
  (>>=) (Identity a) f = f a

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

instance (Eq a) => EqProp (Identity a) where (=-=) = eq

identityTrigger :: Identity (String, String, String)
identityTrigger = undefined

-- 4)

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

take' :: Int -> List a -> List a
take' 0 _           = Nil
take' _ Nil         = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

instance Monoid (List a) where
  mempty = Nil
  mappend a Nil          = a
  mappend Nil a          = a
  mappend (Cons x xs) ys = Cons x $ xs `mappend` ys

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _         = Nil
  (<*>) _ Nil         = Nil
  (<*>) (Cons f x) xs = fmap f xs <> (x <*> xs)

instance Monad List where
  return = pure
  (>>=) Nil _         = Nil
  (>>=) (Cons x xs) f = f x <> (xs >>= f)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [(1, return Nil),
               (10, return (Cons x y))]

instance Eq a => EqProp (List a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = take' 3000 xs
          ys' = take' 3000 ys

listTrigger :: List (String, String, String)
listTrigger = undefined

-- Writing functions

-- 1)
j :: Monad m => m (m a) -> m a
j = join

-- 2)
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = liftM

-- 3)
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

-- 4)
a :: Monad m => m a -> m (a -> b) -> m b
a = flip ap

-- 5)
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _     = pure []
meh (x:xs) f = (:) <$> (f x) <*> (meh xs f)
{-  10:[13] (Just . show)  = (:) <$> (Just "10") ...
 -                         = Just ("10" : ) <*> (meh [13] (Just . show))
 -                         = Just ("10" : "13") <*> (meh [] (Just . show))
 -                         = Just ("10" : "13" : [])
 -                         = Just (["10", "13"])
 -}

-- 6)
flipType :: (Monad m) => [m a] -> m [a]
flipType xs = meh xs id
{- [Just 10, Just 13] id  = (:) <$> Just (10 : ) <*> (meh [Just 13] id)
 -                        = Just (10 : 13 :) <*> (meh [] id)
 -                        = Just (10 : 13 : [])
 -                        = Just ([10,13])
 -}

test :: IO()
test = do

  putStrLn "Nope Tests"
  quickBatch $ monad nopeTrigger
  quickBatch $ functor nopeTrigger
  quickBatch $ applicative nopeTrigger

  putStrLn "PhhhbbtttEither Tests"
  quickBatch $ monad phhhbbtttEitherTrigger
  quickBatch $ functor phhhbbtttEitherTrigger
  quickBatch $ applicative phhhbbtttEitherTrigger

  putStrLn "Identity Tests"
  quickBatch $ monad identityTrigger
  quickBatch $ functor identityTrigger
  quickBatch $ applicative identityTrigger

  putStrLn "List Tests"
  quickBatch $ monad listTrigger
  quickBatch $ functor listTrigger
  quickBatch $ applicative listTrigger


