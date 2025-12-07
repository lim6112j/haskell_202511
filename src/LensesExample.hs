{-# LANGUAGE TemplateHaskell #-}

module LensesExample (someFunc5, advancedLensExample, alice, name) where

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
  }
  deriving (Show)

data Department = Department
  { _deptName :: String,
    _budget :: Int,
    _manager :: Maybe User
  }
  deriving (Show)

makeLenses ''Company
makeLenses ''Department

sampleCompany :: Company
sampleCompany = Company "TechCorp" [alice, alice2] [Department "Engineering" 100000 (Just alice)]

advancedLensExample :: IO ()
advancedLensExample = do
  putStrLn "=== Advanced Lens Examples ==="
  putStrLn $ "sampleCompany: " ++ show sampleCompany
  putStrLn $ "companyName: " ++ show (sampleCompany ^. companyName)
  putStrLn $ "employees: " ++ show (sampleCompany ^. employees)
  putStrLn $ "departments: " ++ show (sampleCompany ^. departments)
  putStrLn $ "deptName: " ++ show (sampleCompany ^.. departments . traverse . deptName)
  putStrLn $ "budget: " ++ show (sampleCompany ^.. departments . traverse . budget)
  putStrLn $ "manager: " ++ show (sampleCompany ^.. departments . traverse . manager)
  putStrLn $ "manager name: " ++ show (sampleCompany ^.. departments . traverse . manager . _Just . name)
  putStrLn $ "manager address: " ++ show (sampleCompany ^.. departments . traverse . manager . _Just . address)
  putStrLn $ "manager street: " ++ show (sampleCompany ^.. departments . traverse . manager . _Just . address . street)
  putStrLn $ "manager street name: " ++ show (sampleCompany ^.. departments . traverse . manager . _Just . address . street . streetName)
  putStrLn $ "manager zip code: " ++ show (sampleCompany ^.. departments . traverse . manager . _Just . address . zipCode)
  putStrLn "=== End of Advanced Lens Examples ==="

someFunc5 :: IO ()
someFunc5 = do
  let user = User {_name = "John", _address = Address {_zipCode = "12345", _street = Street {_streetName = "Main Street"}}}
  let updatedUser = user & address . street . streetName .~ "New Street"
  print updatedUser
  print $ "address of user : " ++ user ^. address . street . streetName
  print $ user & address . street . streetName .~ "New St"
  print alice2
