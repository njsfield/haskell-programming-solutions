module ChapterExercises where

import Data.List (sort)

{-
 - Multiple Choice
 - 
 - 1. The Eq class
 -   c) makes equality tests possible 
 - 
 - 2. The typeclass Ord
 -   a) allows any two values to be compared
 -   d) is a subclass of Eq
 -
 - 3. Suppose the typeclass Ord has an operator >. 
 -    What is the type of >?
 -   a) Ord a => a -> a -> Bool
 -
 - 4. In x = divMod 16 12
 -   c) the type of x is a tuple
 -
 - 5. The typeclass Integral includes
 -   a) Int and Integer numbers
 -}


{-
 - Does it typecheck?
 -
 -}

-- 1. Does not typecheck. Resolved 
data Person = Person Bool 
              deriving (Show)

printPerson :: Person -> IO()
printPerson person = putStrLn (show person)

-- 2. Does not typecheck
data Mood = Blah
          | Woot deriving (Eq,Show)

settleDown x = if x == Woot
               then Blah
               else x

-- 3. 
-- a) Values accepted for settlDown are Blah or Woot 
-- b) settleDown 9 will throw Error (no Eq instance for Num,Mood)
-- c) Blah > Woot will throw error, no instance of Ord

-- 4.
type Subject = String
type Verb    = String
type Object  = String

data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"
-- Works because Subject Verb and Object are type alias's for string

{-
 - Given a datatype declaration, what can we do?
 -}

data Rocks =
  Rocks String deriving (Eq, Show)
 
data Yeah =
  Yeah Bool deriving (Eq, Show)

data Papu =
  Papu Rocks Yeah 
  deriving (Eq, Show)

-- 1. Will not type check (cannot match Yeah with String- fix: Yeah "chases")
-- phew = Papu "chases" True

-- 2. Will type check
truth = Papu (Rocks "chomskydoz") (Yeah True)

-- 3. Will type check (as Eq typeclass derived)
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- 4. Will not type check (no instance of Ord)
-- comparePapus :: Papu -> Papu -> Bool
-- comparePapus p p' = p > p'

{-
 - Match the Types
 -}

-- 1 (Does not work when replaced with i :: a, needs binding Num a => a)
i :: Num a => a
i = 1

-- 2 (Does not work when replaced with Num a => a, cannot reduce from Num) 
f :: Float
f = 1.0

-- 3 (Works when replaced with Fractional a => a)
f' :: Float
f' = 1.0

-- 4 (Works when replaced with RealFrac a => a)
f'' :: Float
f'' = 1.0

-- 5 (Works when replaced with Ord a => a -> a)
freud :: a -> a
freud x = x

-- 6 (Whens when replaced with Int -> Int)
freud' :: a -> a
freud' x = x

-- 7 (Does not work when replaced with a -> a)
myX = 1 :: Int
sigmund :: Int -> Int
sigmund x = myX

-- 8 (Does not work when replaced with Num a => a -> a)
myX' = 1 :: Int
sigmund' :: Int -> Int
sigmund' x = myX'

-- 9 (Works when replaced with [Int] -> Int) 
jung :: Ord a => [a] -> a
jung xs = head (sort xs)

-- 10 (Works when replaced with Ord a => [a] -> a)
young ::  Ord a => [a] -> a
young xs = head (sort xs)

-- 11  (Does not work when replaced with Ord a => [a] -> a)
mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs) 


{-
 - Type-Kwon-Do Two: Electric Typealoo
 -}

-- 1
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = f a == b 

-- 2 (using fromInteger)
arith :: Num b => (a -> b) -> Integer -> a -> b
arith f i a = f a + fromInteger i
