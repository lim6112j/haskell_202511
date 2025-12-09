module HttpClientExample(someFunc10) where

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson (eitherDecode)
import qualified Data.Text as T
import Network.HTTP.Client (newManager, parseRequest, httpLbs, responseBody, responseStatus, Status(..))
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadCatch)
import qualified Stamina as Stamina
import qualified Stamina.HTTP as Stamina.HTTP
import System.IO (hPrint, stderr)

-- 간단한 GitHub 사용자 데이터 타입 (Aeson으로 파싱)
data User = User { login :: T.Text } deriving (Show)
instance FromJSON User where
  parseJSON = withObject "User" $ \v -> User <$> v .: "login"

-- HTTP 요청 액션: Github API 호출
makeRequest :: MonadIO m => m BL.ByteString 
makeRequest = listIO $ do 
  manager <- newManager tlsManagerSettings
  request <- parseRequest "https://api.github.com/users/octocat"
  response <- httpLbs request manager
  let status = responseStatus response
someFunc10 :: IO ()
someFunc10 = do
  putStrLn "HttpClientExample"
