{-# LANGUAGE FlexibleContexts #-}

module MtlExample (someFunc3) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Text.Array (new)

data AppConfig = AppConfig
  { initialCounter :: Int,
    messagePrefix :: String
  }
  deriving (Show)

newtype AppState = AppState
  { currentCounter :: Int
  }
  deriving (Show)

appLogic :: (MonadReader AppConfig m, MonadState AppState m) => m String
appLogic = do
  config <- ask
  let prefix = messagePrefix config
  AppState current <- get
  let newCounter = current + 1
  put (AppState newCounter)
  return $ prefix ++ " Counter: " ++ show newCounter

someFunc3 :: IO ()
someFunc3 = do
  let config = AppConfig {initialCounter = 10, messagePrefix = "Hello from app!"}
  let initialState = AppState {currentCounter = initialCounter config}

  -- Run the ReaderT layer, then the StateT layer, finally in IO
  let computation = runStateT (runReaderT appLogic config) initialState
  (result, finalState) <- computation

  putStrLn result
  putStrLn $ "Final Counter: " ++ show (currentCounter finalState)
