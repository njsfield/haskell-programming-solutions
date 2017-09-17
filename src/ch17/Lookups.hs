module Lookups where

import           Data.List (elemIndex)

-- 1

added :: Maybe Integer
added = (+ 3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

-- zip [1, 2, 3] [4, 5, 6] == [(1, 4), (2, 5), (3, 6)]
-- lookup 3 [(1, 4), (2, 5), (3, 6)] == Just 6
-- fmap (+3) Just 6 == Just 9
-- (+3) <$> Just 6 == Just 9

-- 2

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- y = Just 6
-- z = Just 5
-- Just ((6,)) <*> Just 5 == Just (6,5)
-- fmap (,) Just 6 == Just ((6,))
-- (,) <$> Just 6 <*> Just 5 == Just (6,5)
-- ((,) <$> Just 6) <*> Just 5 == Just (6,5)

-- 2
x' :: Maybe Int
x' = elemIndex 3 [1, 2, 3, 4, 5]

y' :: Maybe Int
y' = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x' <*> y'

-- x' = Just 2
-- y' = Just 3
-- (Just (max' 2)) <*> (Just 3) == Just 3
-- fmap (max') Just 2 == Just (max' 2)
-- max' <$> x' <*> y' == Just 3
-- (max' <$> x') <*> y' == Just 3

-- 4
xs = [1,2,3]
ys = [4,5,6]

x'' :: Maybe Integer
x'' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = (<$>) sum $ (,) <$> x'' <*> y''

-- x'' = Just 6
-- y'' = Just 5
-- (,) x'' y'' = (Just 6, Just 5)
-- (,) <$> x'' <*> y'' = Just (6,5)
-- fmap sum (Just (6,5)) = Just 5
-- (<$>) sum $ (,) <$> x'' <*> y''


