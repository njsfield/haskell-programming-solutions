module ChapterExercises where

import           Data.Bool
import           Data.Char
import           Data.Char
import           Data.List (intersperse)
{-
 - Multiple Choice
 -}

-- 1)

data WeekDay =
    Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday

-- We can say;
-- a) Weekday is a type with five data constructors

-- 2)

f Friday = "Miller Time"

-- F's type is
--    c) f :: Weekday -> String

-- 3) Types defined with the data keyword
--    b) must begin with a capital letter

-- 4) The function g xs = xs !! (length xs - 1)
--    c) delivers the final element of xs


{-
 - Cipher
 -}

type Keyword = String
type Message = String

type KeywordHead = Char
type MessageHead = Char

-- Transpose a char by the other chars A-Z mapping to 0-26
-- (works with Capitals only)
transposed :: MessageHead -> KeywordHead -> MessageHead
transposed mh kh = chr
                 . flip (+) 65
                 . flip rem 26
                 . flip (-) 65
                 . (+) (ord kh - 65)
                 . ord
                 $ mh

cipher :: Message -> Keyword -> Message
cipher [] _                = []
cipher m@(' ':ms) k        = ' ' : cipher ms k
cipher m@(mh:ms)  k@(kh:ks) = res : cipher ms (ks ++ [kh])
    where res = transposed (toUpper mh) (toUpper kh)

{-
 - As-patterns
 -}


-- 1
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _      = True
isSubsequenceOf (ah:at) b = elem ah b && isSubsequenceOf at b

-- 2
capitalizeWords :: String -> [(String, String)]
capitalizeWords = map (\s@(sh:st) -> (s , toUpper sh : st) ) .  words

{-
 - Language exercises
 -}

-- 1

capitalizeWord :: String -> String
capitalizeWord []      = []
capitalizeWord (sh:st) = toUpper sh : st

-- 2
splitAtChar :: Char -> String -> [String]
splitAtChar _ []  = []
splitAtChar c s@(sh:st) = if c == sh then splitAtChar c st else res
         where first    = takeWhile (/= c) s
               last     = dropWhile (/= c) s
               res      = first : splitAtChar c last

capitalizeParagraph :: String -> String
capitalizeParagraph = concat
                    . intersperse ". "
                    . map capitalizeWord
                    . map (dropWhile (== ' '))
                    . splitAtChar '.'

{-
 - DaPhone
 -}


-- 1) Structure

type Digit        = Char
type Presses      = Int
type DigitPresses = [(Digit,Presses)]

-- Phone is list is possible buttons
data Button  = Button Digit String
data DaPhone = DaPhone [Button]

-- Hold collection of Buttons to capture
-- associated letters (and numbers themselves)
-- String lists are ordered to represent number of taps
daPhone :: DaPhone
daPhone = DaPhone
  [ Button '1' "1",     Button '2' "abc2",  Button '3' "def3"
  , Button '4' "ghi4",  Button '5' "jkl5",  Button '6' "mno6"
  , Button '7' "pqrs7", Button '8' "tuv8",  Button '9' "wxyz9"
  ]


-- 2)

-- Simple build util
dp :: Digit -> Presses -> DigitPresses
dp d p = [(d,p)]


-- Main map function
-- If char is capital, append ('*', 1) to
-- resulting list to indicate capitalization
reverseTaps :: DaPhone -> Char -> DigitPresses
reverseTaps _ '0' = dp '0' 1
reverseTaps _ '+' = dp '0' 2
reverseTaps _ ' ' = dp '0' 3
reverseTaps _ '#' = dp '#' 1
reverseTaps _ '.' = dp '#' 2
reverseTaps _ ',' = dp '#' 3
reverseTaps phone c
  | isUpper c = ('*', 1) : reverseTaps phone (toLower c)
  | otherwise = buttonToDigitPress c
              . findButton c
              $ phone

-- Find a button in a list, based on unique char
findButton :: Char -> DaPhone -> Button
findButton c (DaPhone b) = head
               . filter (\(Button _ chrs) -> elem c chrs)
               $ b

-- Convert button to DigitPresses (with char index)
buttonToDigitPress :: Char -> Button -> DigitPresses
buttonToDigitPress c (Button n chrs) = [(n, (+1) $ elemIndexSimple c chrs)]

-- Get index of element in list
-- (Return 0 if not found)
-- Simpler version of List utility
-- which returns Maybe
elemIndexSimple :: Eq a => a -> [a] -> Int
elemIndexSimple _ []    = 0
elemIndexSimple x xs    = go x xs 0
  where go _  [] count  = count
        go x' (xh':xt') count
         | x' == xh'    = count
         | otherwise    = go x' xt' count + 1

-- For each letter of string
-- Apply reverseTaps and return
-- flattened result
cellPhonesDead :: DaPhone
               -> String
               -> [(Digit, Presses)]
cellPhonesDead phone s = concat
                       . map (reverseTaps phone)
                       $ s

-- Result
convo :: [String]
convo =
  ["Wanna play 20 questions",
  "Ya",
  "U 1st haha",
  "Lol ok. Have u ever tasted alcohol lol",
  "Lol ya",
  "Wow ur cool haha. Ur turn",
  "Ok. Do u think I am pretty Lol",
  "Lol ya",
  "Haha thanks just making sure rofl ur turn"]

convoTaps = concat
          . map (cellPhonesDead daPhone)
          $ convo

-- 3) Sum together presses
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\(_, a) b -> b + a) 0

totalFingerTaps :: String -> Presses
totalFingerTaps s = fingerTaps
                  . concat
                  . map (reverseTaps daPhone)
                  $ s

-- 436 total taps needed
convoTapsTotal = fingerTaps convoTaps


-- 4. Most Popular

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = foldl (\seen x -> if x `elem` seen
                                     then seen
                                     else seen ++ [x]) []

--mostPopular :: String -> Char
mostPopular :: Eq a => [a] -> a
mostPopular s = fst
              . foldr (\(a,b) (c,d) -> if b > d then (a,b) else (c,d))  (head s, 0)
              . map (\x -> (,) x . length . filter ((==) x) $ s)
              . removeDuplicates
              $ s

-- Cost Workout
mostPopularCost s = totalFingerTaps
                  . (\x -> filter (== x) s)
                  . mostPopular $ s

-- Coolest
coolestLtr :: [String] -> Char
coolestLtr = mostPopular . concat

coolestWord :: [String] -> String
coolestWord = mostPopular





{-
 - Huttons Razor
 -}

-- 1

data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit i)   = i
eval (Add x y) = (eval x) + (eval y)


-- 2

printExpr :: Expr -> String
printExpr (Lit i)   = show i
printExpr (Add x y) = (printExpr x) ++ " + " ++ (printExpr y)
