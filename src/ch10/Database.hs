module Ch10.Database where

import           Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
           (fromGregorian 1911 5 1)
           (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

-- 1)
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate db = [ x | (DbDate x) <- db]

-- 2)
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber db = [ x | (DbNumber x) <- db]

-- 3) (maximum :: Ord a -> t a -> a)
--    (UTCTime has Ord typeclass instance)
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

-- 4)
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

-- 5)
avgDb :: [DatabaseItem] -> Double
avgDb db = (fromIntegral (sumDb db)) / (foldl (flip (const (+1))) 0 (filterDbNumber db))
