module KmeansExample where

import qualified Vector.Unboxed as U
import Vector.Unboxed ((!), Unbox)
import Contol.Monad.Primitive
import Control.Monad.ST

kmeans :: Int -> Int -> U.Vector (Float, Float) -> U.Vector (Float, Float)
kmeans k iters points = runST $ do
  cents <- initialCentroids k points 
  loop iters cents points

  where
    loop 0 cents _ = return cents
    loop iters cents points = do
      assignments <- assignPoints cents points
      newCents <- updateCentroids assignments points
      loop (iters - 1) newCents points
