module Ch04.ChapterExercises where

awesome = ["Papuchon", "curry", ":)"]

alsoAwesome = ["Quake", "The Simons"]

allAwesome = [awesome, alsoAwesome]

{-
 - 1. length :: Foldable t => t a -> Int 
 - 2. 
 -   a) length [1,2,3,4,5] == 5
 -   b) length [(1,2), (2,3), (3,4)] == 3
 -   c) length allAwesome == 2
 -   d) length (concat allAwesome) == 5
 - 
 - 3. 
 -   6 / 3 (compiles - types resolved to Floats) 
 -   6 / length [1, 2, 3] (fails expected Float for right value, but length returns Int)
 -
 - 4. How can you fix 6 / length [1, 2, 3] ?
 -
 -   a) Use div function. 6 `div` length [1, 2, 3]
 -
 - 5. (2 + 3 == 5) :: Bool (True)
 - 6. Prelude> let x = 5; x + 3 ==5 (Bool) (True)
 - 7. 
 -    length allAwesome == 2  (works: True)
 -    length [1, 'a', 3, 'b'] (broken; mixed types)
 -    length allAwesome + length awesome (works: 5)
 -    (8 == 8) && ('b' < 'a') (works: False)
 -    (8 == 8) && 9 (broken: expected right side to be Bool)
 -}

-- 8.
isPalindrome x = x == reverse x

-- 9.
myAbs :: Integer -> Integer
myAbs x =
  if x < 0
    then negate x
    else x

-- 10.
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a, b) (c, d) = ((b, d), (a, c))

{--
 -
 - Correcting syntax
 - =================
 -
 -}

-- 1.
x = (+)

f' xs = w `x` 1
  where
    w = length xs

-- 2. 
identity' = \x -> x

-- 3. 
first' = \(x:xs) -> x

-- 4.
f'' (a, b) = a

{-
 -
 - Match the function names to their types
 - =======================================
 - 
 - 1. c) Show a => a -> String 
 - 2. b) Eq a => a -> a -> Bool
 - 3. a) (a, b) -> a
 - 4. d) (+) :: Num a => a -> a -> a
 -}
