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


someFunc10 :: IO ()
someFunc10 = do
  putStrLn "HttpClientExample"
