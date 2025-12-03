{-# LANGUAGE TemplateHaskell #-}

module TemplateExample (someFunc4) where

import qualified TemplateImporting as TH

plus5 :: Int -> Int
plus5 = $(TH.makePlusN 5)

someFunc4 :: IO ()
someFunc4 = do
  let n = 2
  putStrLn $ "template sum of 5 + 2 : " ++ show (plus5 n)
