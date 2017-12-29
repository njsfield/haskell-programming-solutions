module Ch24.SemVer where

import Text.Trifecta
import Control.Applicative

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


main :: IO ()
main = do
  print $ parseString parseSemVer mempty "2.1.1"
  print $ parseString parseSemVer mempty "1.0.0-x.7.z.92"
  print $ parseString parseSemVer mempty "4.2.1-z.2017+0.0.0"
  print $ SemVer 2 1 1 [][] > SemVer 2 1 0 [][]
