{-# LANGUAGE OverloadedStrings #-}

module PersistentExample (someFunc8) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist
import Database.Persist.Sqlite (runSqlite, runMigration)
import Schema

someFunc8 :: IO ()
someFunc8 = do
  putStrLn "=== Persistent Examples ==="
  runStderrLoggingT $ runSqlite ":memory:" $ do
    runMigration migrateAll
    insert_ $ Person "Alice" 25
    insert_ $ Person "Bob" 30
    people <- selectList [] []
    liftIO $ print (people :: [Entity Person])
  putStrLn "=== End of Persistent Examples ==="
