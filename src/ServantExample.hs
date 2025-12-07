module ServantExample (someFunc7) where

import Data.Text (Text)
import MyAPI
import Network.Wai.Handler.Warp (run)
import Servant

server :: Server MyAPI
server = helloHandler :<|> greetHandler
  where
    helloHandler :: Handler Text
    helloHandler = pure "Hello, servant!"

    greetHandler :: Text -> Handler Text
    greetHandler name = pure $ "Hello, " <> name <> "!"

app :: Application
app = serve (Proxy :: Proxy MyAPI) server

someFunc7 :: IO ()
someFunc7 = do
  putStrLn "=== Servant Examples ==="
  run 8080 app
  putStrLn "=== End of Servant Examples ==="
