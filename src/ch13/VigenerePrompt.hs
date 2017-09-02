module Ch13.VigenerePrompt where

import Ch11.Vigenere (vigenere)

userVig :: IO String
userVig = do
  putStrLn "Please enter a message and press enter: "
  word <- getLine
  putStrLn "Now enter a keyword to transpose your message with: "
  word' <- getLine
  return (vigenere word word')
