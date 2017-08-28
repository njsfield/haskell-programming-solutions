module ChapterExercises where

import           Data.Bool
{-
 - Warm-up & review
 -}

-- 1)

stops  = "pbtdkg"
vowels = "aeiou"

-- a)
toTuples as bs = [(a,b,c) | a <- as , b <- bs, c <- as]
stopVowelStops = toTuples stops vowels

-- b)
beginsWithP = filter (\(a,_,_) -> a == 'p') $ stopVowelStops

-- c)
nouns = ["car", "horse", "duck", "chair", "mountain"]
verbs = ["run", "swim", "race", "drive", "climb"]

nounVerbnouns = toTuples nouns verbs


-- 2)
-- What does it do? Returns average number of letters per word
-- What is its type? String -> Int

-- (Rewritten for fractional division)
seekritFunc :: Fractional a => String -> a
seekritFunc x =
  (/) (fromIntegral (sum (map length (words x))))
      (fromIntegral (length (words x)))

{-
 - Rewriting functions using folds
 -}

-- 1)
myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- 2)
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False

-- 3)
myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr ((||) . (== x)) False
myElem' x = any (== x)

-- 4)
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- 5)
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

-- 6)
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\b a -> if f b then b : a else a ) []

-- 7)
squish :: [[a]] -> [a]
squish = foldr (flip $ foldr (:)) []

-- 8)
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\b a -> foldr (:) a (f b)) []

-- 9)
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 10)
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldr (\a b -> if (f a b == GT) then a else b) (last xs) xs

-- 11)
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldr (\a b -> if (f a b == LT) then a else b) (last xs) xs



