{-# LANGUAGE QuasiQuotes #-}

module Ch24.Log where

import           Control.Applicative
import           Data.Map
import           Data.Time
import           Text.RawString.QQ
import           Text.Trifecta

data Log =
  Log Day (Map TimeOfDay String) deriving (Eq,Show)

testLog :: String
testLog = [r|

-- wheee a comment

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

fullLogParser :: Parser [Log]
fullLogParser = some logParser

logParser :: Parser Log
logParser = do
  _       <- skipNewLines
  _       <- skipComments
  _       <- skipNewLines
  d       <- dayParser
  xs      <- some activityParser
  return (mapDayAndActivities d xs)

mapDayAndActivities :: Day -> [(TimeOfDay, String)] -> Log
mapDayAndActivities d xs =
  Log d (fromList xs)

dayParser :: Parser Day
dayParser = do
  string "# "
  y <- integer
  _ <- char '-'
  m <- integer
  _ <- char '-'
  d <- integer
  _ <- skipComments
  return (fromGregorian y (fromInteger m) (fromInteger d))

activityParser :: Parser (TimeOfDay, String)
activityParser = do
  h <- some digit
  _ <- char ':'
  m <- some digit
  _ <- char ' '
  s <- some (token (try letter <|> try (oneOf ",- &?")))
  return (TimeOfDay {
            todHour = read h
          , todMin = read m
          , todSec = 0}
          , s)

skipNewLines :: Parser ()
skipNewLines =
  skipMany (oneOf "\n")

skipComments :: Parser ()
skipComments =
  skipMany (string "-- " >> some (token (some letter)))

main :: IO ()
main = do
  print "Parse Both Days: "
  print $ parseString fullLogParser mempty testLog
