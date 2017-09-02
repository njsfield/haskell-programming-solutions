module Ch09.Eft where

-- (Assuming the role eft must have 
-- its first argument be lower than 
-- the second, otherwise an empty
-- list is returned)

-- (1 possibility)
eftBool :: Bool -> Bool -> [Bool]
eftBool False True  = [False,True]
eftBool _ _         = []

-- (3 possibilities)
eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd LT EQ = [LT,EQ]
eftOrd LT GT = [LT,EQ,GT]
eftOrd EQ GT = [EQ,GT]
eftOrd _  _  = []

-- (Custom)

eftAny :: (Enum a, Ord a) => a -> a -> [a]
eftAny x y
  | x > y     = []
  | otherwise = [x] ++ (eftAny (succ x) y) 

eftInt :: Int -> Int -> [Int]
eftInt = eftAny

eftChar :: Char -> Char -> [Char]
eftChar = eftAny
