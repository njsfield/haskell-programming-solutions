{-# LANGUAGE FlexibleContexts #-}

module ChapterExercises where

import           Control.Monad
import           Data.Foldable
import           Data.Monoid
import           Data.Traversable
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

-- 1) Identity'

newtype Identity' a = Identity' a
  deriving (Eq, Ord, Show)

instance Functor Identity' where
  fmap f (Identity' a) = Identity' (f a)

instance Applicative Identity' where
  pure = Identity'
  Identity' f <*> Identity' a = Identity' (f a)

instance Foldable Identity' where
  foldMap f (Identity' _) = mempty

instance Traversable Identity' where
  traverse f (Identity' a) = Identity' <$> f a

instance Arbitrary a => Arbitrary (Identity' a) where
  arbitrary = do
    a <- arbitrary
    return (Identity' a)

instance Eq a => EqProp (Identity' a) where (=-=) = eq

identityTrigger = undefined :: Identity' (Int, Int, [Int])

-- 2)

newtype Constant a b = Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap f (Constant _) = mempty

instance Traversable (Constant a) where
  traverse f (Constant a) = pure (Constant a)
--
instance (Arbitrary a, Arbitrary b) => Arbitrary (Constant a b) where
  arbitrary = do
    a <- arbitrary
    return (Constant a)

instance (Eq a, Eq b) => EqProp (Constant a b) where (=-=) = eq

constantTrigger = undefined :: Constant Int (Int, Int, [Int])

-- 3)
data Optional a =
    Nada
  | Yep a
   deriving (Eq, Show)

instance Functor Optional where
  fmap _ Nada    = Nada
  fmap f (Yep a) = Yep (f a)

instance Foldable Optional where
  foldMap f Nada    = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse _ Nada    = pure Nada
  traverse f (Yep a) = Yep <$> f a

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    elements [ Nada, Yep a ]

instance Eq a => EqProp (Optional a) where (=-=) = eq

optionalTrigger = undefined :: Optional (Int, Int, [Int])

-- 4)
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Monoid (List a) where
  mempty = Nil
  mappend a Nil                = a
  mappend Nil a                = a
  mappend (Cons a lista) listb = Cons a $ lista <> listb

instance Functor List where
  fmap _ Nil            = Nil
  fmap f (Cons a lista) = Cons (f a) (fmap f lista)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _                 = Nil
  (<*>) _ Nil                 = Nil
  (<*>) (Cons fa lista) listb = fmap fa listb <> (lista <*> listb)

instance Foldable List where
  foldMap f Nil            = mempty
  foldMap f (Cons a lista) = f a <> foldMap f lista

  foldr f x Nil            = x
  foldr f x (Cons a lista) = f a (foldr f x lista)

instance Traversable List where
  traverse f Nil            = pure Nil
  traverse f (Cons a lista) = Cons <$> f a <*> traverse f lista

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = genList

genList :: Arbitrary a => Gen (List a)
genList = do
  a <- arbitrary
  b <- genList
  frequency [(1, return Nil), (3, return $ Cons a b)]

instance Eq a => EqProp (List a) where (=-=) = eq

listTrigger = undefined :: List (Int, Int, [Int])

-- 5)
data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

threeTrigger = undefined :: Three Int Int (Int, Int, [Int])

-- 6)
data Three' a b =
  Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance Foldable (Three' a) where
  foldMap f (Three' _ b b') = f b <> f b'

instance Traversable (Three' a) where
  traverse f (Three' a b b') = Three' a <$> f b <*> f b'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return (Three' a b b')

instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

threeBTrigger = undefined :: Three' Int (Int, Int, [Int])

-- 7)
data S n a = S (n a) a
  deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S n a) = S (fmap f n) (f a)

instance Foldable n => Foldable (S n) where
  foldMap f (S n a) = foldMap f n <> f a

instance Traversable n => Traversable (S n) where
  traverse f (S n a) = S <$> traverse f n <*> f a

instance (Eq (n a), Eq a) => EqProp (S n a) where
  (=-=) = eq

instance (Arbitrary (n a), CoArbitrary (n a),
          Arbitrary a, CoArbitrary a) =>
          Arbitrary (S n a) where
  arbitrary = do
    n <- arbitrary
    a <- arbitrary
    return $ S (n a) a

snaTrigger = undefined :: S Maybe (Int, Int, [Int])

-- Tree

data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap f Empty                = Empty
  fmap f (Leaf a)             = Leaf (f a)
  fmap f (Node treel a treer) = Node (fmap f treel) (f a) (fmap f treer)

instance Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node treel a treer) =
    foldMap f treel <> f a <> foldMap f treer

instance Traversable Tree where
  traverse f Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node treel a treer) =
    Node <$> traverse f treel <*> f a <*> traverse f treer

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = genTree

genTree :: Arbitrary a => Gen (Tree a)
genTree = do
  a <- arbitrary
  treel <- genTree
  treer <- genTree
  frequency [ (1, return Empty)
            , (2, return $ Leaf a)
            , (2, return $ Node treel a treer) ]

treeTrigger = undefined :: Tree (Int, Int, [Int])

main :: IO ()
main = do
  putStr "\nIdentity'"
  quickBatch (traversable identityTrigger)
  putStr "\nConstant"
  quickBatch (traversable constantTrigger)
  putStr "\nOptional"
  quickBatch (traversable optionalTrigger)
  putStr "\nOptional"
  quickBatch (traversable optionalTrigger)
  putStr "\nOptional"
  quickBatch (traversable optionalTrigger)
  putStr "\nList"
  quickBatch (traversable listTrigger)
  putStr "\nThree"
  quickBatch (traversable threeTrigger)
  putStr "\nThree'"
  quickBatch (traversable threeBTrigger)
  putStr "\nS"
  quickBatch (traversable snaTrigger)
  putStr "\nTree"
  quickBatch (traversable treeTrigger)
