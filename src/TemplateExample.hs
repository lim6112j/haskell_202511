{-# LANGUAGE TemplateHaskell #-}

module TemplateExample (someFunc4) where

import Control.Monad.RWS (MonadState (put))
import qualified TemplateImporting as TH

plus5 :: Int -> Int
plus5 = $(TH.makePlusN 5)

simpleCalc :: Int
simpleCalc = $(TH.simpleQuote)

greetAction :: IO ()
greetAction = $(TH.exprQuote "Hello from quasi-quote!")

someFunc4 :: IO ()
someFunc4 = do
  let n = 2
  putStrLn $ "template sum of 5 + 2 : " ++ show (plus5 n)
  putStrLn $ "simple calc: " ++ show simpleCalc
  greetAction
