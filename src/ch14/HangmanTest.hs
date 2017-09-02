module Ch14.HangmanTest where

import Test.Hspec

import Ch13.Hangman 

-- Current Puzzle

currentPuzzle :: Puzzle
currentPuzzle = (Puzzle "the" [Just 't', Nothing, Nothing] "t")

-- Case correct
correctGuess :: Puzzle
correctGuess   = fillInCharacter currentPuzzle 'h' 

-- Case incorrect
incorrectGuess :: Puzzle
incorrectGuess = fillInCharacter currentPuzzle 'i' 

main :: IO ()
main =
  hspec $ do
    describe "fillInCharacter: " $ do
      -- Test constants
      (Puzzle _ oldFilled oldGuessed) <- return $ currentPuzzle
      (Puzzle _ newFilledCorrect newGuessedCorrect) <- return $ correctGuess
      (Puzzle _ newFilledIncorrect newGuessedIncorrect) <- return $ incorrectGuess

      -- Correct
      it "Correct guess adds to filledIn Maybe List" $ do 
        newFilledCorrect `shouldNotBe` oldFilled
      it "Correct guess adds to guessed List" $ do 
        newGuessedCorrect `shouldNotBe` oldGuessed
        (length newGuessedCorrect == length oldGuessed + 1) `shouldBe` True

      -- Incorrect
      it "Incorrect guess does not modify Maybe List" $ do 
        newFilledIncorrect `shouldBe` oldFilled
      it "Incorrect guess does not modify guessed List" $ do 
        newGuessedIncorrect `shouldNotBe` oldGuessed
        (length newGuessedIncorrect == length oldGuessed) `shouldBe` False

