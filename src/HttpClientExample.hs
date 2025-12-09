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
  if statusCode status == 429
    then fail "Rate limited (429)"
    else return $ responseBody response

-- 재시도 핸들러: 예회 발생 시 로그 출력 및 상태 확인
logRetryStatus :: MonadIO m => Stamina.RetryStatus -> m ()
logRetryStatus status = liftIO $ do
  let attempts = Stamina.attempts status
      delay = Stamina.delay status
      lastExc = Stamina.lastException status
  hPrint stderr $ "Retry attempt " ++ show attempts ++ " after " ++ show delay ++ "s. last error: " ++ show lastExc

-- 메인 재시도 로직: Stamina.HTTP.retry 사용
retryHttpExample :: IO ()
retryHttpExample = Stamina.HTTP.retry settings $ \retryStatus -> do
  logRetryStatus retryStatus
  body <- makeRequest
  case eitherDecode body :: Either String User of
    Left err -> fail $ "JSON decode error: " ++ err
    Right user -> liftIO $ print user

settings :: Stamina.RetrySettings
settings = Stamina.defaults
  { Stamina.maxAttempts = Just 5
  }

someFunc10 :: IO ()
someFunc10 = do
  putStrLn "=== HTTP Client Example with Stamina Retry ==="
  retryHttpExample 
  putStrLn "=== End of HTTP Client Example ==="
