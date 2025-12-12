{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module OpticsHotReloadExample where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Data.Generics.Product (field)
import GHC.Generics (Generic)
import Optics
import Rapid (Rapid, createRef, rapid)
import Relude

-- | Nested configuration example
data DatabaseConfig = DatabaseConfig
  { dbHost :: Text
  , dbPort :: Int
  }
  deriving (Show, Eq, Generic)

data ServerConfig = ServerConfig
  { serverPort :: Int
  , dbConfig :: DatabaseConfig
  }
  deriving (Show, Eq, Generic)

-- | Initial configuration
initialConfig :: ServerConfig
initialConfig =
  ServerConfig
    { serverPort = 8080
    , dbConfig =
        DatabaseConfig
          { dbHost = "localhost"
          , dbPort = 5432
          }
    }

-- | Run the application with hot reload support
--
-- > runExample
runExample :: IO ()
runExample = rapid 0 $ \rpd -> do
  putStrLn "Starting Optics + Rapid Hot Reload Example"

  -- Create or retrieve the mutable config reference
  -- 'createRef' ensures this IORef persists across reloads if the key matches
  ref <- createRef rpd "global-config" (newIORef initialConfig)

  -- Start a background daemon
  -- In a real app, you might want to use 'start' from Rapid to manage threads
  void $ forkIO $ forever $ do
    cfg <- readIORef ref
    putStrLn $ "Current Config: " <> show cfg
    threadDelay 1000000 -- 1 second

  putStrLn "Daemon started. You can update config in GHCi."
  putStrLn "To update config, use: updateConfig rpd (#dbConfig % #dbPort) 9000"
  
  -- Keep the main thread running for a bit
  threadDelay 5000000

-- | Update the configuration using optics
updateConfig :: (Is k A_Setter, JoinKinds k A_Setter k) => Rapid String -> Optic k is ServerConfig ServerConfig a b -> b -> IO ()
updateConfig rpd optic newValue = do
  ref <- createRef rpd "global-config" (newIORef initialConfig)
  modifyIORef' ref $ \cfg -> cfg & optic .~ newValue
  putStrLn "Configuration updated!"

main :: IO ()
main = runExample
