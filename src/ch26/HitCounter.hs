{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.IORef
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as TL
import Data.Text.Lazy.Builder (fromString, toLazyText)
import           System.Environment         (getArgs)
import           Web.Scotty.Trans

data Config =
  Config {
  -- that's one, one click!
  -- two...two clicks!
  -- Three BEAUTIFUL clicks! ah ah ahhhh
    counts :: IORef (M.Map Text Integer)
  , prefix :: Text
  }
-- Stuff inside ScottyT is, except for things that escape
-- via IO, effectively read-only so we can't use StateT.
-- It would overcomplicate things to attempt to do so and
-- you should be using a proper database for production
-- applications.

type Scotty = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text
          -> M.Map Text Integer
          -> (M.Map Text Integer, Integer)
bumpBoomp k m =
  case M.lookup k m of
    Just i  -> (M.update (Just . (+1)) k m, i + 1)
    Nothing -> (M.insert k 1 m, 1)

app :: Scotty ()
app =
  get "/:key" $ do
    unprefixed <- param "key"
    config <- lift ask
    let key = prefix config `mappend` unprefixed
        ref = counts config
        m   = readIORef ref
    (updatedMap, i) <- liftIO $ fmap (bumpBoomp key) m
    liftIO $ writeIORef ref updatedMap
    html $ mconcat [ "<h1>Success! Count was: "
                   , TL.pack $ show i
                   , "</h1>"
                   ]

main :: IO ()
main = do
  [prefix] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter (toLazyText . fromString $ prefix)
      runR r = runReaderT r config
  scottyT 3000 runR app
