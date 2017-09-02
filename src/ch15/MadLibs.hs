module Ch15.MadLibs where

import Data.Monoid

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation
           -> Adverb
           -> Noun
           -> Adjective
           -> String

madlibbin' e adv noun adj =
  e    <> "! he said " <>
  adv  <> " a he jumped into his car " <>
  noun <> " and drove off with his " <>
  adj  <> " wife." 


-- Refactored with mconcat

madlibbinBetter' :: Exclamation
                 -> Adverb
                 -> Noun
                 -> Adjective
                 -> String
madlibbinBetter' e adv noun adj = mconcat 
  [ e,    "! he said " 
  , adj,  " as he jumped into his car "
  , noun, " and drove off with his "
  , adj,  " wife" ]
    
