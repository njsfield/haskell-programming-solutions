module Ch09.ChapterExercises where

import Data.Bool
import Data.Char

{-
 - Data.Char
 -}

{- 1)
 -
 - isUpper :: Char -> Bool
 - toUpper :: Char -> Char
 -}

-- 2) (use isUpper)

onlyUppers :: String -> String
onlyUppers = filter isUpper

-- 3)
capitalizeFirst :: String -> String
capitalizeFirst (s:t) = [toUpper s] ++ t

-- 4)
capitalizeAll :: String -> String
capitalizeAll []    = []
capitalizeAll (s:t) = [toUpper s] ++ capitalizeAll t

-- 5) head :: [a] -> a
firstAsCap :: String -> Char
firstAsCap = head . capitalizeFirst


{-
 - Ciphers
 -}

-- (From Chapter 13)



{-
 - Writing your own standard functions
 -}

-- 1)
myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs

-- 2)
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ []     = False
myAny f (x:xs) = f x || myAny f xs

-- 3)
myElem :: Eq a => a -> [a] -> Bool
myElem _ []     = False
myElem c (x:xs) = c == x || myElem x xs

-- 4)
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

-- 5)
squish :: [[a]] -> [a]
squish []     = []
squish (x:xs) = x ++ squish xs

-- 6)
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ []     = []
squishMap f (x:xs) = f x ++ squishMap f xs

-- 7)
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 8)
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs)   = currentMax f x xs
   where
     currentMax _ y [] = y
     currentMax f y (z:zs)
       | f y z == GT   = currentMax f y zs
       | otherwise     = currentMax f z zs
-- 9)
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs)   = currentMax f x xs
   where
     currentMax _ y [] = y
     currentMax f y (z:zs)
       | f y z == LT   = currentMax f y zs
       | otherwise     = currentMax f z zs

-- 10)
myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare



