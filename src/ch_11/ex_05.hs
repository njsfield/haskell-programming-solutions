{-# LANGUAGE FlexibleInstances #-}

module ExerciseFive where

class TooMany a where
  tooMany :: a -> Bool

-- 1

instance TooMany Cows where
  tooMany (Cows (n, _)) = n > 42

newtype Cows =
  Cows (Int, String) deriving Show

-- 2

newtype Herd =
  Herd (Int, Int) deriving Show

instance TooMany Herd where
  tooMany (Herd (x, y)) = (x + y) > 42

-- 3

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (x, y) = tooMany $ x + y



