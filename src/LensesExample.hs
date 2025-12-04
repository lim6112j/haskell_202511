{-# LANGUAGE TemplateHaskell #-}

module LensesExample (someFunc5) where

import Control.Lens
import Data.Maybe (fromMaybe)

newtype Street = Street {_streetName :: String} deriving (Show)

data Address = Address {_zipCode :: String, _street :: Street} deriving (Show)

data User = User {_name :: String, _address :: Address} deriving (Show)

makeLenses ''Street
makeLenses ''Address
makeLenses ''User

alice :: User
alice = User "Alice" (Address "12345" (Street "Old St"))

alice2 :: User
alice2 =
  alice
    & name .~ "Alice Kim"
    & address . zipCode .~ "54321"
    & address . street . streetName %~ ("Fancy " ++)

data Company = Company
{ _companyName :: String,
  _employees :: [User],
  _departments :: [Department]
} deriving (Show)

data Department = Department
{
  _deptName :: String,
  _budget :: Int,
  _manager :: Maybe User
} deriving (Show)

makeLenses ''Company
makeLenses ''Department

someFunc5 :: IO ()
someFunc5 = do
  let user = User {_name = "John", _address = Address {_zipCode = "12345", _street = Street {_streetName = "Main Street"}}}
  let updatedUser = user & address . street . streetName .~ "New Street"
  print updatedUser
  print $ "address of user : " ++ user ^. address . street . streetName
  print $ user & address . street . streetName .~ "New St"
  print alice2
