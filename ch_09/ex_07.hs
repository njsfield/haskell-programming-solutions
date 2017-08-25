module ExerciseSeven where

{-
 - Filtering
 -}

-- 1
threeMults :: Integral a => [a] -> [a]
threeMults lst = filter (\x -> rem x 3 == 0) lst

-- 2
threeMultsCount :: Integral a => [a] -> Int
threeMultsCount = length . threeMults

-- 3
myFilter :: String -> [String]
myFilter = filter (\x -> not . elem x $ ["the", "a", "an"]) . words
