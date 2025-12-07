{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module MyAPI () where

import Data.Text (Text)
import Servant

type MyAPI =
  "hello" :> Get '[JSON] Text
    :<|> "greet" :> Capture "name" Text :> Get '[JSON] Text
