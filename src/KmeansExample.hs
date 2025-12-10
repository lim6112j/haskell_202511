module KmeansExample where

import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed ((!), Unbox)
import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad (forM_)
import Data.List (minimumBy)
import Data.Ord (comparing)

type Point = (Float, Float)

kmeans :: Int -> Int -> U.Vector Point -> U.Vector Point
kmeans k iters points = runST $ do
  cents <- initialCentroids k points 
  loop iters cents points

  where
    loop 0 cents _ = return cents
    loop n cents pts = do
      let assignments = assignPoints cents pts
      let newCents = updateCentroids k assignments pts
      loop (n - 1) newCents pts

-- Initialize centroids by taking first k points
initialCentroids :: PrimMonad m => Int -> U.Vector Point -> m (U.Vector Point)
initialCentroids k points = return $ U.take k points

-- Assign each point to nearest centroid
assignPoints :: U.Vector Point -> U.Vector Point -> U.Vector Int
assignPoints centroids points = U.map findNearest points
  where
    findNearest p = fst $ minimumBy (comparing snd) 
                    [(i, distance p (centroids ! i)) | i <- [0..U.length centroids - 1]]
    distance (x1, y1) (x2, y2) = (x1 - x2)^(2::Int) + (y1 - y2)^(2::Int)

-- Update centroids based on assignments
updateCentroids :: Int -> U.Vector Int -> U.Vector Point -> U.Vector Point
updateCentroids k assignments points = U.generate k computeCentroid
  where
    computeCentroid i = 
      let clusterPoints = U.filter (\idx -> assignments ! idx == i) (U.enumFromN 0 (U.length points))
          clusterSize = U.length clusterPoints
      in if clusterSize == 0
         then (0, 0)  -- default if no points assigned
         else let (sumX, sumY) = U.foldl' (\(sx, sy) idx -> 
                                    let (x, y) = points ! idx
                                    in (sx + x, sy + y)) (0, 0) clusterPoints
              in (sumX / fromIntegral clusterSize, sumY / fromIntegral clusterSize)
