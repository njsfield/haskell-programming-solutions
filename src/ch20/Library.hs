module Ch20.Library where

import           Control.Monad
import           Data.Foldable
import           Data.Monoid

-- 1
sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

-- 2
product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

-- 3
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' a = getAny . foldMap (Any . (== a))

-- 4
newtype Min a =
  Min { getMin :: Maybe a } deriving (Eq, Show)

instance Ord a => Monoid (Min a) where
  mempty = Min Nothing
  mappend (Min Nothing) x  = x
  mappend x (Min mempty)   = x
  mappend (Min a) (Min a') = Min (min a a')

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' x = getMin $ foldMap (Min . Just) x

-- 5
newtype Max a =
  Max { getMax :: Maybe a } deriving (Eq, Show)

instance Ord a => Monoid (Max a) where
  mempty = Max Nothing
  mappend (Max Nothing) x  = x
  mappend x (Max mempty)   = x
  mappend (Max a) (Max a') = Max (max a a')

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' x = getMax $ foldMap (Max . Just) x

-- 6
null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> False) True

-- 7
length' :: (Foldable t) => t a -> Int
length' = foldr (\_ b -> b + 1) 0

-- 8
toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

-- 9
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

-- 10
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (mappend . f) mempty
