module ChapterExercises where

{-
 - Multiple Choice
 - 
 - 1) A polymorphic function
 -
 -   d) may resolve to values of different types, depending on inputs
 -
 -
 - 2) b) Char -> [String]
 -
 - 3) d) (Ord a, Num a) => a -> Bool 
 -
 - 4) b) is a higher-order function
 -
 - 5) a) f True :: Bool
 -}

{-
 - Let's write code
 -}

-- 1

tensDigit :: Integral a => a -> a
tensDigit x   = d
  where xLast = x `div` 10
        d     = xLast `mod` 10

-- a)
tensDigit' :: Integral a => a -> a
tensDigit' x       = d
  where (xLast, _) = divMod x 10
        (_, d)     = divMod xLast 10

-- b) Has the same type
-- c) 
hundredsDigit :: Integral a => a -> a
hundredsDigit = tensDigit . (`div` 10) 


-- 2
foldBool :: a -> a -> Bool -> a
foldBool x y condition =
  case condition of
    True  -> x
    False -> y 

foldBool' :: a -> a -> Bool -> a
foldBool' x y condition
  | condition = x
  | otherwise = y

-- 3
g :: (a -> b) -> (a, c) -> (b, c)
g f (a,c) = (f a, c)

-- 4

{-
 - Works because roundTrip converts
 - val back to * type
 - 
 - Print accepts * as type argument
 -
 -}
roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

main = do
  print (roundTrip 4)
  print (id 4)


-- 5 (point free)

roundTrip' :: (Show a, Read a) => a -> a
roundTrip' = read . show


-- 6
roundTrip'' :: (Show a, Read b) => a -> b
roundTrip'' = read . show

resolved = roundTrip (4 :: Int)

 


