{-# LANGUAGE OverloadedStrings #-}

module Ch26.Scotty where

import Web.Scotty
import Web.Scotty.Internal.Types (ActionT(..))
import Data.Monoid (mconcat)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy hiding (get)


main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    (ActionT
     . (ExceptT . fmap Right)
     . ReaderT . const
     . \m -> StateT (\s -> do
                        a <- m
                        return (a, s))
     ) (putStrLn "hello")
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
