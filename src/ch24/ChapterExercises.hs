module Ch24.ChapterExercises where

import Text.Trifecta
import Control.Applicative

-- 1

data NumberOrString =
    NOSS String
  | NOSI Integer
  deriving (Eq, Ord, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata
  deriving (Show, Eq)

instance Ord SemVer where
  compare (SemVer maA miA paA _ _)
          (SemVer maB miB paB _ _) =
    let
      semTotal ma mi pa
        = (ma * 100)
        + (mi * 10)
        + pa
    in
      compare (semTotal maA miA paA)
              (semTotal maB miB paB)
  (>) sa sb =
    compare sa sb == GT

parserNumberOrString :: Parser NumberOrString
parserNumberOrString =
  (NOSS <$> try (some letter)) <|> (NOSI <$> try integer)

parserNumOrStringDot :: Parser NumberOrString
parserNumOrStringDot = do
  nos <- parserNumberOrString
  _   <- skipMany (oneOf ".")
  return nos

parseSemVer :: Parser SemVer
parseSemVer = do
  ma <- integer
  mi <- char '.' >> integer
  pa <- char '.' >> integer
  _  <- option '.' (char '-')
  re <- option [] (some parserNumOrStringDot)
  _  <- option '.' (char '+')
  me <- option [] (some parserNumOrStringDot)
  return (SemVer ma mi pa re me)

-- 2 & 3

parseDigit :: Parser Char
parseDigit = oneOf "0123456789"

base10Integer :: Parser Integer
base10Integer = do
  s <- option ' ' (char '-')
  (read . (++) [s] <$> some parseDigit) <?> "integer"

-- 4
type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
  PhoneNumber NumberingPlanArea Exchange LineNumber
  deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = do
  let flexDigit = skipMany (oneOf "()- ")
                >> digit
  xs <- try (string "1-" >> some flexDigit)
    <|> try (some flexDigit)
    <|> try (some digit)
  return (PhoneNumber
      (read $ take 3 xs)
      (read $ (take 3 . drop 3) xs)
      (read $ drop 6 xs))

main :: IO ()
main = do
  print "parseSemVer Tests: "
  print $ parseString parseSemVer mempty "2.1.1"
  print $ parseString parseSemVer mempty "1.0.0-x.7.z.92"
  print $ parseString parseSemVer mempty "4.2.1-z.2017+0.0.0"
  print $ SemVer 2 1 1 [][] > SemVer 2 1 0 [][]
  print "parseDigit Tests: "
  print $ parseString parseDigit mempty "123"
  print $ parseString parseDigit mempty "abc"
  print "base10Integer Tests: "
  print $ parseString base10Integer mempty "123abc"
  print $ parseString base10Integer mempty "-123abc"
  print $ parseString base10Integer mempty "abc"
  print "parsePhone Tests: "
  print $ parseString parsePhone mempty "123-456-7890"
  print $ parseString parsePhone mempty "1234567890"
  print $ parseString parsePhone mempty "(123) 456-7890"
  print $ parseString parsePhone mempty "1-123-456-7890"
