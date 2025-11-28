module Main (main) where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Lib

main :: IO ()
main = do
  let s1 = Set.fromList [1, 2, 3, 4, 5]
      s2 = Set.fromList [3, 4, 5, 6, 7]
      s3 = Set.union s1 s2
      s4 = Set.difference s1 s2
      s5 = Set.intersection s1 s2
      s6 = Set.difference s2 s1
      s7 = Set.difference s3 s1
      datastructure = Set.fromList ["Set", "Map", "Graph", "Sequence"]
      hasMap = Set.member "Map" datastructure
  putStrLn $ "Hello World!"
  putStrLn $ "s1: " ++ show s1
  putStrLn $ "s2: " ++ show s2
  putStrLn $ "s3: " ++ show s3
  putStrLn $ "s4: " ++ show s4
  putStrLn $ "s5: " ++ show s5
  putStrLn $ "s6: " ++ show s6
  putStrLn $ "s7: " ++ show s7
  putStrLn $ "datastructure: " ++ show datastructure
  putStrLn $ "hasMap: " ++ show hasMap
  someFunc
