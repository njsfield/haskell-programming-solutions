module Ch12.ChapterExercises where

import Data.Bool
import Data.List (intersperse)

{-
 - Determine The Kinds
 -}

-- 1) Given
--   id :: a -> a
--   The kind of a is *

-- 2) Given
--   r :: a -> f a
--
--   The kind of a is *
--   The kind of f is * -> *


{-
 - String processing
 -}

-- 1)

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s     = Just s

replaceThe :: String -> String
replaceThe = concat
           . intersperse " "
           . map (maybe "a" id . notThe)
           . words

-- 2)

vowels = "aeiou"

-- utils
isVowel :: Char -> Bool
isVowel = flip elem vowels

isConsonant :: Char -> Bool
isConsonant = not . isVowel

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel []  = 0
countTheBeforeVowel str = snd . foldr theAndVowelPair base $ rest
    where (start:rest)  = words str
          base          = (start,0)


-- Compare the two
theAndVowelPair :: String -> (String, Integer) -> (String, Integer)
theAndVowelPair s@(sh:_) ("the",i) = bool (s,i)(s,i+1)  (isVowel sh)
theAndVowelPair s (_,i)            = (s,i)


-- 3)
countVowels :: String -> Integer
countVowels = foldr (\a b -> bool (b+1) b (isVowel a)) 0


{-
 - Validate the word
 -}

newtype Word' =
  Word' String
  deriving (Eq, Show)

vowelConsonantBalance :: String -> Integer
vowelConsonantBalance = foldr (\a b -> bool (b+1) (b-1) (isVowel a)) 0

mkWord :: String -> Maybe Word'
mkWord s
  | vowelConsonantBalance s < 0 = Nothing
  | otherwise                   = Just (Word' s)

{-
 - It's only Natural
 -}

data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ x) = 1 + (natToInteger x)

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x < 0       = Nothing
  | otherwise   = Just (go x)
    where go 0  = Zero
          go x' = Succ (go (x'-1))

{-
 - Small library for Maybe
 -}

--1)

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _        = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

--2)
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b f (Just a) = f a
mayybee b _ _        = b

--3)
fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing  = a
fromMaybe _ (Just a) = a

--4)
listToMaybe :: [a] -> Maybe a
listToMaybe []      = Nothing
listToMaybe (lh:lt) = Just lh

maybeToList :: Maybe a -> [a]
maybeToList (Just a) = [a]
maybeToList Nothing  = []

--5)
catMaybes :: [Maybe a] -> [a]
catMaybes = map (\(Just a) -> a) . filter isJust

{-
 - Small library for Either
 -}

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

fromLeft :: Either a b -> a
fromLeft (Left a) = a

isRight :: Either a b -> Bool
isRight = not . isLeft

fromRight :: Either a b -> b
fromRight (Right b) = b

--1)
lefts' :: [Either a b] -> [a]
lefts' = foldr ((:) . fromLeft) [] . filter isLeft

--2)
rights' :: [Either a b] -> [b]
rights' = foldr ((:) . fromRight) [] . filter isRight

--3)
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' eithers = (lefts' eithers, rights' eithers)

--4)
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _)  = Nothing
eitherMaybe' f (Right a) = Just (f a)

--5)
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a)  = f a
either' _ f (Right b) = f b

--6)
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' _ (Left _)  = Nothing
eitherMaybe'' f (Right b) = Just (either' id f (Right b))


{-
 - Write your own iterate and unfoldr
 -}

-- 1)
myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

-- 2)
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = go (f b)
  where go (Just (a', b')) = a' : go (f b')
        go Nothing         = []

-- 3)
betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\b -> Just (b, f b)) x


{-
 - Finally something other than a list!
 -}

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

-- 1)

unfoldTree :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfoldTree f a = go (f a)
  where go (Just (a',b',a'')) = Node (go (f a')) b' (go (f a''))
        go Nothing            = Leaf


-- 2)
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfoldTree (\a -> if a == n then Nothing else (Just (a+1, a, a+1))) 0



