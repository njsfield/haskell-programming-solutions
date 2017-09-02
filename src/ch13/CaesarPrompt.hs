module Ch13.CaesarPrompt where

import Ch09.Caesar 

userCaeser :: IO String
userCaeser = do
  putStrLn "Please enter a word to shift to the right: "
  word <- getLine
  putStrLn $ "How many places shall I shift " ++ word ++ " to the right?"
  places <- getLine
  return (caesarShiftRight (read places) word)
