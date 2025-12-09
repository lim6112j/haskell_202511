{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
module CrawlerExample where

import Control.Concurrent (getNumCapabilities, threadDelay)
import Control.Concurrent.Async (mapConcurrently_, async, wait)
import Control.Concurrent.STM
import Control.Monad (forever, forM_, void, when)
import Data.Set (Set)
import qualified Data.Set as Set
import Network.HTTP.Client
import Network.HTTP.Client.TLS (newTlsManager)
import Network.URI (URI(..), URIAuth(..), parseURI)
import Text.HTML.Scalpel.Core
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Text as T
import GHC.Generics (Generic)

type UrlQueue = TQueue T.Text
type Visited = TVar (Set T.Text)

newVisited :: IO Visited
newVisited = newTVarIO Set.empty

markVisited :: Visited -> T.Text -> STM Bool
markVisited visited url = do
  visitedSet <- readTVar visited
  if Set.member url visitedSet
    then return False
    else do
      writeTVar visited (Set.insert url visitedSet)
      return True

someFunc11 :: IO ()
someFunc11 = do
  putStrLn "someFunc11"
