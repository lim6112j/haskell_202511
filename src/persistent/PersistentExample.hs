{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module PersistentExample (someFunc8) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist hiding (update, delete, (==.), (=.))
import Database.Persist.Sqlite (runSqlite)
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
    persons <- select $ do
        p <- from $ table @Person
        return p
    liftIO $ putStrLn "All Persons:"
    liftIO $ mapM_ print persons

    -- Example 2: Select persons named "John Doe"
    johns <- select $ do
        p <- from $ table @Person
        where_ (p ^. PersonName ==. val "John Doe")
        return p
    liftIO $ putStrLn "\nPersons named John Doe:"
    liftIO $ mapM_ print johns

    -- Example 3: Select blog posts with their author's name
    postsWithAuthors <- select $ do
        (p :& bp) <- from $ table @Person `innerJoin` table @BlogPost `on` do
            \(p :& bp) -> p ^. PersonId ==. bp ^. BlogPostPersonId
        orderBy [desc (bp ^. BlogPostCreatedAt)]
        return (p ^. PersonName, bp ^. BlogPostTitle)
    liftIO $ putStrLn "\nBlog Posts with Authors:"
    liftIO $ mapM_ print postsWithAuthors

    -- Example 4: Update a person's age
    update $ \p -> do
        set p [PersonAge =. val 31]
        where_ (p ^. PersonName ==. val "John Doe")
    liftIO $ putStrLn "\nJohn's age updated."

    -- Example 5: Delete a person (first delete their blog posts to avoid foreign key constraint)
    -- Delete Jane's blog posts first using a subquery
    delete $ do
        bp <- from $ table @BlogPost
        where_ $ bp ^. BlogPostPersonId `in_` subList_select (do
            p <- from $ table @Person
            where_ (p ^. PersonName ==. val "Jane Smith")
            return (p ^. PersonId))
    -- Now delete Jane
    delete $ do
        p <- from $ table @Person
        where_ (p ^. PersonName ==. val "Jane Smith")
    liftIO $ putStrLn "\nJane Smith and her blog posts deleted."

    -- Verify changes
    remainingPersons <- select $ do
        p <- from $ table @Person
        return p
    liftIO $ putStrLn "\nRemaining Persons:"
    liftIO $ mapM_ print remainingPersons
  putStrLn "=== End of Persistent Examples ==="
