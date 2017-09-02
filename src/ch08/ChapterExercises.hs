module Ch08.ChapterExercises where

{-
 - Review of types
 -
 - 1) [[True, False], [True, True], [False, True]]
 -
 -    d) [[Bool]]
 - 
 - 2) b) [[3 == 3], [6 > 5], [3 < 4]]
 - 
 - 3) func :: [a] -> [a] -> [a]
 -    func x y = x ++ y
 -
 -    a) x and y must be of the same type
 -    b) x and y must both be lists
 -    c) if x is a String then y must be a string
 -
 -    All of the above
 -
 -  4) a) func "Hello World"
 -}

{-
 - Reviewing currying
 -}

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"


-- 1 "woops mrow woohoo!"
cat_1 = appedCatty "woohoo!"

-- 2 "1 mrow haha"
cat_2 = frappe "1"

-- 3 "woops mrow 2 mrow haha" 
cat_3 = frappe (appedCatty "2")

-- 4 "woops mrow blue mrow haha"
cat_4 = appedCatty (frappe "blue")

-- 5 "pink mrow haha mrow green mrow woops mrow blue"
cat_5 = cattyConny (frappe "pink")
                   (cattyConny "green" (appedCatty "blue"))

-- 6 "are mrow Pugs mrow awesome"
cat_6 = cattyConny (flippy "Pugs" "are") "awesome"


{-
 - Recursion
 -} 
 
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n   d count
         | n < d     = (count, n)
         | otherwise = go (n - d) d (count + 1)
 
{- 1)
 - dividedBy 15 2 =
 -    go 15 2 0 
 -    go 13 2 1
 -    go 11 2 3
 -    go  9 2 4
 -    go  7 2 5
 -    go  5 2 6
 -    go  3 2 7
 -    (recursion stops- 1 is less than 2)
 -    Result tuple = (7,1)
 -}

-- 2
mySum :: (Eq a, Num a) => a -> a
mySum val = go 1 val 0 
  where go start value total
         | start == value  = total + start
         | otherwise       = go (start + 1) value (total + start)


-- 3
myMult :: (Integral a) => a -> a -> a
myMult x y = matchSign y (go x y 0) 
  where go x y total
         | y == 0          = total 
         | otherwise       = go x (to0 y) (total + x)   
        to0 z
         | z > 0     = z - 1
         | z < 0     = z + 1
         | otherwise = 0
        matchSign a b
         | a < 0     = negate . abs $ b 
         | otherwise = b

{-
 - Fixing dividedBy
 -}

data DividedResult a =
    Result a
  | DividedByZero

dividedBy' :: Integral a => a -> a -> DividedResult a
dividedBy' num denom
  | denom == 0                  = DividedByZero
  | signum num == signum denom  = Result r
  | otherwise                   = Result (-r)
  where
    r = go (abs num) (abs denom) 0
    go n d count
      | n < d     = count
      | otherwise = go (n - d) d (count + 1)


{-
 - McCarthy 91 function
-}

mc91 :: (Ord a, Num a) => a -> a
mc91 n 
  | n >  100  = n - 10
  | n <= 100  = mc91 (mc91 (n + 11))

{-
 - Numbers into words
 -
 -}

-- (See NumbersToWords.hs)
