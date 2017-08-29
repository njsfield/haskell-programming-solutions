module ChapterExercises where

import           Control.Monad
import           Data.Char     (toLower)
import           System.Exit   (exitSuccess)
{-
 - Hangman Game logic (additions)
 -
 - Requirements
 - ============
 -
 - 1. Guess count should equal target word character length
 - 2. Game should only factor in INCORRECT guesses when
 -    deciding how many guesses you have left
 - 3. If last guess results in correct word, game will still
 -    tell you you lost
 - 4. Game shouldn't use such obscure words
 -
 -
 - Solutions
 - =========
 - 1. In gameOver function, the if statement should evaluate against
 -    length of target word
 - 2. In gameOver function, instead of simply comparing length
 -    of guessed chars, modify the statement to subtract the total
 -    correct chars found from the already guessed list.
 -    This can be achieved by filtering the Just values from the
 -    filledInSoFar Maybe list, and then using the length of that array
 - 3. Swap gameWin & gameOver calls in runGame (so that gameWin is checked first)
 - 4. N/A - Use different word library (various sources available online)
 -
 -}

{-
 - Modifying code
 -}


-- 1) See ch_09 (userCeasar) & ch_11 (userVig)

-- 2) (Add exist success to prevent infinite loop)

palindrome :: IO()
palindrome = forever $ do
  line1 <- getLine
  case (line1 == reverse line1) of
    True  -> do
      putStrLn "It's a palindrome!"
      exitSuccess
    False -> putStrLn "Nope"

-- 3) (Works on sentences)
palindrome' :: IO()
palindrome' = forever $ do
  line1 <- getLine
  lowerLine1 <- return (map toLower line1)
  case (lowerLine1 == reverse lowerLine1) of
    True  -> do
      putStrLn "It's a palindrome!"
      exitSuccess
    False -> putStrLn "Nope"


-- 4)

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                   deriving (Eq, Show)
mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
                       "Name was: " ++ show name ++
                       " Age was: " ++ show age


gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Please enter your name: "
  name <- getLine
  putStrLn "Please enter your age: "
  age <- getLine
  case mkPerson name (read age) of
    Right (Person n a) ->
      putStrLn $ "Yay! Successfully go a person: " ++ name ++ ", " ++ age
    Left NameEmpty     ->
      putStrLn "Invalid! Name is empty"
    Left AgeTooLow     ->
      putStrLn $ "Invalid! " ++ age ++ " is too low"
    Left (PersonInvalidUnknown e) ->
      putStrLn $ "Invalid! " ++ e

