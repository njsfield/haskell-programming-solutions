module Ch24.Phone where

import Text.Trifecta
import Control.Applicative

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
  print $ parseString parsePhone mempty "123-456-7890"
  print $ parseString parsePhone mempty "1234567890"
  print $ parseString parsePhone mempty "(123) 456-7890"
  print $ parseString parsePhone mempty "1-123-456-7890"
