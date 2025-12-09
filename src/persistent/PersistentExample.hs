{-# LANGUAGE OverloadedStrings #-}

module PersistentExample (someFunc8) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist hiding (update, delete, (==.), (=.))
import Database.Persist.Sqlite (runSqlite, runMigration)
import Database.Esqueleto.Experimental
import Data.Time (getCurrentTime)
import Schema

someFunc8 :: IO ()
someFunc8 = do
  putStrLn "=== Persistent Examples ==="
  runStderrLoggingT $ runSqlite ":memory:" $ do
    runMigration migrateAll
    -- Insert some data
    johnId <- insert $ Person "John Doe" 30
    janeId <- insert $ Person "Jane Smith" 25
    currentTime <- liftIO getCurrentTime
    _ <- insert $ BlogPost johnId "My First Post" "Content of John's first post." currentTime
    _ <- insert $ BlogPost janeId "Jane's Blog" "Hello from Jane!" currentTime

    -- Example 1: Select all persons
    persons <- select $ from $ \p -> do
        return p
    liftIO $ putStrLn "All Persons:"
    liftIO $ mapM_ print persons

    -- Example 2: Select persons named "John Doe"
    johns <- select $ from $ \p -> do
        where_ (p ^. PersonName ==. val "John Doe")
        return p
    liftIO $ putStrLn "\nPersons named John Doe:"
    liftIO $ mapM_ print johns

    -- Example 3: Select blog posts with their author's name
    postsWithAuthors <- select $ from $ \(p `InnerJoin` bp) -> do
        on (p ^. PersonId ==. bp ^. BlogPostPersonId)
        orderBy [desc (bp ^. BlogPostCreatedAt)]
        return (p ^. PersonName, bp ^. BlogPostTitle)
    liftIO $ putStrLn "\nBlog Posts with Authors:"
    liftIO $ mapM_ print postsWithAuthors

    -- Example 4: Update a person's age
    update $ \p -> do
        set p [PersonAge =. val 31]
        where_ (p ^. PersonName ==. val "John Doe")
    liftIO $ putStrLn "\nJohn's age updated."

    -- Example 5: Delete a person
    delete $ from $ \p -> do
        where_ (p ^. PersonName ==. val "Jane Smith")
    liftIO $ putStrLn "\nJane Smith deleted."

    -- Verify changes
    remainingPersons <- select $ from $ \p -> return p
    liftIO $ putStrLn "\nRemaining Persons:"
    liftIO $ mapM_ print remainingPersons
  putStrLn "=== End of Persistent Examples ==="
