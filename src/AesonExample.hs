{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module AesonExample (someFunc6) where

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy.Char8 as L8
import GHC.Generics (Generic)

data Person = Person
  { name :: String,
    age :: Int,
    likesPizza :: Bool
  }
  deriving (Show, Generic, FromJSON, ToJSON)

someFunc6 :: IO ()
someFunc6 = do
  putStrLn "=== Aeson Examples ==="
  let jsonString = "{\"name\":\"John\",\"age\":30,\"likesPizza\":true}"
  case decode jsonString :: Maybe Person of
    Just person -> putStrLn $ "Decoded person: " ++ show person
    Nothing -> putStrLn "Failed to decode JSON"
  let bob = Person {name = "Bob", age = 25, likesPizza = False}
  let encodedJson = encode bob
  putStrLn $ "Encoded Json of Bob: " ++ L8.unpack encodedJson

  -- Example with a list of People
  let peopleJsonString = "[{\"name\":\"Charlie\",\"age\":40,\"likesPizza\":true},{\"name\":\"David\",\"age\":35,\"likesPizza\":false}]"
  case decode peopleJsonString :: Maybe [Person] of
    Just people -> putStrLn $ "Decoded people: " ++ show people
    Nothing -> putStrLn "Failed to decode JSON"
  putStrLn "=== End of Aeson Examples ==="
