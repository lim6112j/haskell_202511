{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module CrawlerExample where

import Control.Concurrent (getNumCapabilities, threadDelay)
import Control.Concurrent.Async (Async, async, mapConcurrently_, wait)
import Control.Concurrent.STM
import Control.Monad (forM_, forever, void, when)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (newTlsManager)
import Network.URI (URI (..), URIAuth (..), parseURI)
import Text.HTML.Scalpel.Core
import qualified Stamina
import qualified Stamina.HTTP

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

workerCount :: Int -> IO Int
workerCount maxConcurrent = do
  caps <- getNumCapabilities
  pure $ max 256 (min maxConcurrent (caps * 64))

spawnWorkers :: Int -> (Int -> IO ()) -> IO [Async ()]
spawnWorkers n action = mapM (async . action) [1 .. n]

-- url 정규화

normalizeUrl :: T.Text -> T.Text
normalizeUrl =
  T.strip
    . T.pack
    . (\u -> u {uriPath = dropTrailingSlash (uriPath u)})
    . fromMaybe (error "invalid url")
    . parseURI
    . T.unpack
  where
    dropTrailingSlash p | lastMay p == '/' = init p
                        | otherwise = p
    lastMay xs | null xs = Nothing | otherwise = Just (last xs)

settings :: Stamina.RetrySettings
settings = Stamina.defaults
  { Stamina.maxAttempts = Just 5
  }

sameDomain :: T.Text -> T.Text -> Bool
sameDomain base url = case (parseURI (T.unpack base), parseURI (T.unpack url)) of
  (Just b, Just u) -> uriRegName (fromMaybe (URIAuth "" "" "") (uriAuthority b))
                   == uriRegName (fromMaybe (URIAuth "" "" "") (uriAuthority u))
  _ -> False

-- crawling logic

crawlPage :: Manager -> UrlQueue -> Visited -> T.Text -> IO ()
crawlPage manager urlQueue visited url = do
  putStrLn $ "[Crawling] " ++ T.unpack url
  -- stamina429/5xx 자동 재시도 
  result <- Stamina.HTTP.retry settings 
  $ do
    req <- parseRequest (T.unpack url)
    resp <- httpLbs req manager
    let body = responseBody resp
    pure (responseStatus resp, BL.toStrict body)
  case result of
    Left err -> putStrLn $ "[Error] " ++ show err
    Right (status, body) -> do
      when (status == status200) $ do
        let links = catMaybes $ scrapeStringLike body $ chroots ("a" @: []) $ attr "href"
        forM_ links $ \link -> do
          let normalized = normalizeUrl link
          when (sameDomain url normalized) $ do
            isVisited <- atomically $ markVisited visited normalized
            when isVisited $ atomically $ writeTQueue urlQueue normalized
  where
    allLinks :: Scraper T.Text [T.Text]
    allLinks = chroots ("a" @: [hasClass "href"]) $ attr "href" anySelector

    makeAbsolute :: T.Text -> T.Text -> T.Text
    makeAbsolute base rel =
      case parseURI (T.unpack rel) of
        Just u | isAbsoluteURI (T.unpack rel) -> T.pack (show u)
        _ -> T.pack $ uriToString id (relativeTo (fromJust $ parseURI $ T.unpack rel)
                                               (fromJust $ parseURI $ T.unpack base)) ""

    enqueueIfNew :: UrlQueue -> Visited -> T.Text -> STM Bool
    enqueueIfNew q v u = do
      ok <- markVisited v (normalizeUrl u)
      when ok $ writeTQueue q (normalizeUrl u)
      return ok
someFunc11 :: IO ()
someFunc11 = do
  putStrLn "someFunc11"
