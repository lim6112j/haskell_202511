{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ServantExample (someFunc7) where

import Data.Text (Text)
import Network.Wai.Handler.Warp (run)
import Servant

type MyAPI = "hello" :> Get '[JSON] Text
        :<|> "greet" :> Capture "name" Text :> Get '[JSON] Text

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
