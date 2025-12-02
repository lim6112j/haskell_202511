{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module ContainerLib
  ( someFunc,
  )
where

import qualified Data.Map.Strict as Map
import Data.Sequence ((<|), (|>))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

someFunc :: IO ()
someFunc = do
  let s1 = Set.fromList [1, 2, 3, 4, 5]
      s2 = Set.fromList [3, 4, 5, 6, 7]
      s3 = Set.union s1 s2
      s4 = Set.difference s1 s2
      s5 = Set.intersection s1 s2
      s6 = Set.difference s2 s1
      s7 = Set.difference s3 s1
      datastructure = Set.fromList ["Set", "Map", "Graph", "Sequence"]
      hasMap = Set.member "Map" datastructure
      m1 = Map.fromList [("a", 1), ("b", 2), ("c", 3)]
      m2 = Map.delete "a" m1
      seq1 = Seq.fromList [1, 2, 3, 4, 5]
      seq2 = 0 <| seq1
      seq3 = seq1 |> 6
  putStrLn "Hello World!"
  putStrLn $ "s1: " ++ show s1
  putStrLn $ "s2: " ++ show s2
  putStrLn $ "s3: " ++ show s3
  putStrLn $ "s4: " ++ show s4
  putStrLn $ "s5: " ++ show s5
  putStrLn $ "s6: " ++ show s6
  putStrLn $ "s7: " ++ show s7
  putStrLn $ "datastructure: " ++ show datastructure
  putStrLn $ "hasMap: " ++ show hasMap
  putStrLn $ "m1: " ++ show m1
  putStrLn $ "m2: " ++ show m2
  putStrLn $ "seq1: " ++ show seq1
  putStrLn $ "seq2: " ++ show seq2
  putStrLn $ "seq3: " ++ show seq3
  let myText = T.pack "Hello, world!"
  TIO.putStrLn myText
