module ExerciseEight where

import           Data.List
{-
 - Programmers
 -}


data OperatingSystem =
    GnuPlusLinux
  | OpenBSDPlusNevermindJustDSBStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgrammingLanguage =
    Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer =
  Programmer { os   :: OperatingSystem
             , lang :: ProgrammingLanguage }
  deriving (Eq, Show)

-- 1

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [ GnuPlusLinux
         , OpenBSDPlusNevermindJustDSBStill
         , Mac
         , Windows
         ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [ Haskell
        , Agda
        , Idris
        , PureScript
        ]

-- All possible programmer combinations
allProgrammers :: [Programmer]
allProgrammers = nub [ Programmer x y | x <- allOperatingSystems, y <- allLanguages]


