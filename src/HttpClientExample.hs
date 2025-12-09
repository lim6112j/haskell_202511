{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module HttpClientExample(someFunc10) where

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson (eitherDecode, FromJSON)
import qualified Data.Text as T
import Network.HTTP.Client (newManager, parseRequest, httpLbs, responseBody, responseStatus)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadCatch)
import qualified Stamina as Stamina
import qualified Stamina.HTTP as Stamina.HTTP
import System.IO (hPrint, stderr)
import GHC.Generics (Generic)

-- Simple GitHub user data type (parsed with Aeson)
data User = User { login :: T.Text } deriving (Show, Generic)

instance FromJSON User

-- HTTP request action: Call Github API
makeRequest :: MonadIO m => m BL.ByteString 
makeRequest = liftIO $ do 
  manager <- newManager tlsManagerSettings
  request <- parseRequest "https://api.github.com/users/octocat"
  response <- httpLbs request manager
  let status = responseStatus response
  return $ responseBody response

someFunc10 :: IO ()
someFunc10 = do
  putStrLn "=== HTTP Client Example with Stamina Retry ==="
  
  -- Create a retry settings with exponential backoff
  let retrySettings = Stamina.defaults
  
  -- Execute request with retry logic
  result <- Stamina.retry retrySettings $ \retryStatus -> do
    putStrLn $ "Making HTTP request..."
    body <- makeRequest
    case eitherDecode body :: Either String User of
      Left err -> do
        putStrLn $ "Parse error: " ++ err
        return body
      Right user -> do
        putStrLn $ "Successfully fetched user: " ++ T.unpack (login user)
        return body
  
  putStrLn "=== End of HTTP Client Example ==="
