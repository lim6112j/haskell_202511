{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module AesonLawsExample where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog.Classes (lawsCheck, jsonLaws)
import Relude

-- | Example data type to test
data User = User
  { userId :: Int
  , userName :: Text
  , userActive :: Bool
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Generator for User
genUser :: Gen User

genUser = User
  <$> Gen.int (Range.linear 0 1000)
  <*> Gen.text (Range.linear 1 10) Gen.alpha
  <*> Gen.bool

-- | Run the Aeson roundtrip laws check
-- This function can be called from GHCi or a test suite.
--
-- > checkAesonLaws
checkAesonLaws :: IO Bool
checkAesonLaws = lawsCheck (jsonLaws genUser)

main :: IO ()
main = void checkAesonLaws
