module Ch13.Hangman where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.List (intersperse)
import Data.Maybe (isJust)
import System.Exit (exitSuccess)
import System.Random (randomRIO) 

newtype WordList =
  WordList [String]
  deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where
    gameLength w =
      let l = length (w :: String)
      in l > minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

-- Binding function to randomWord
randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle =
  Puzzle String
         [Maybe Char]
         [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered) ++
    " Guessed so far: " ++ guessed

-- Generate fresh Puzzle
freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (map (const Nothing) s) ""

-- Determine if a char is in a string from Puzzle
-- Product type
charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _) c = elem c s

-- Determine if a char is in an already guessed string
-- from Puzzle
alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ g) c = elem c g

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c : s)
  where
    zipper guessed wordChar guessChar =
      (if wordChar == guessed
         then Just wordChar
         else guessChar)
    newFilledInSoFar = zipWith (zipper c) word filledInSoFar
            -- E.g.
            -- zipWith (zipper 'c') 'cats' [Nothing, Nothing, Nothing, Nothing]
            -- 1) zipper 'c' 'c' Nothing = Just 'c'
            -- 2) zipper 'c' 'a' Nothing = Nothing
            -- 3) zipper 'c' 't' Nothing = Nothing
            -- 4) zipper 'c' 's' Nothing = Nothing
            --
            -- = [Just 'c', Nothing, Nothing, Nothing]
            --
            --
            -- zipWith (zipper 'a') 'adam' [Nothing, Just 'd', Nothing, Nothing]
            -- 1) zipper 'a' 'a' Nothing  = Just 'a'
            -- 2) zipper 'd' 'a' Just 'd' = Just 'd'
            -- 3) zipper 'a' 'a' Just 'a' = Just 'a'
            -- 4) zipper 'a' 'm' Nothing  = Nothing
            --
            -- = [Just 'a', Just 'd', Just 'a', Nothing]

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case ( charInWord puzzle guess
       , alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn
        "You already guessed that character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn
        "This character was in the world, filling in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn
        "This character wasn't in the word, try again"
      return (fillInCharacter puzzle guess)

-- 1. (Hangman game logic alteration) Chance 7 to length of target word
-- 2. (Hangman game logic alteration) Subtract total correct chars
--    from guessed chars
gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess filledInSoFar guessed) =
  if ((length guessed) -
      (length . filter isJust $ filledInSoFar)) >
     (length wordToGuess)
    then do
      putStrLn "You lose!"
      putStrLn $ "The word was: " ++ wordToGuess
      exitSuccess
    else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar
    then do
      putStrLn "You win!"
      exitSuccess
    else return ()

-- Run!
-- 3. (Hangman Game logic) swap gameOver & gameWin
runGame :: Puzzle -> IO ()
runGame puzzle =
  forever $ do
    gameWin puzzle
    gameOver puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
      [c] -> handleGuess puzzle c >>= runGame
      _ -> putStrLn "Your guess must be a single character"

-- Main
main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle

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
