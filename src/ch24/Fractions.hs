{-# LANGUAGE OverloadedStrings #-}

module Ch24.Fractions where
import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

goodFraction = "1/2"
goodFloat = "1.2"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)

parseFloat :: Parser Float
parseFloat = do
  i <- decimal
  char '.'
  d <- decimal
  return $ fromInteger i + fromInteger d / 10

type FloatOrRational =
  Either Float Rational

parseFractionOrFloat :: Parser FloatOrRational
parseFractionOrFloat =
  (Left <$> try parseFloat) <|> (Right <$> try parseFraction)


main :: IO ()
main = do
  print $ parseString parseFraction mempty goodFraction
  print $ parseString parseFloat mempty goodFloat
  print $ parseString parseFractionOrFloat mempty goodFraction
  print $ parseString parseFractionOrFloat mempty goodFloat
