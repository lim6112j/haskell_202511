{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Schema where

import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.TH

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Person
    name Text
    age Int
    UniqueName name
    deriving Show Eq

BlogPost
    personId PersonId
    title Text
    content Text
    createdAt UTCTime default=CURRENT_TIME
    deriving Show Eq
|]
