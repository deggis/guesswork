{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Guesswork.Estimate where

import Control.Arrow
import Data.Ord
import Data.List
import Guesswork.Types
import Guesswork.Math.Statistics
import qualified Guesswork.Transform as TRANSFORM

data (Sample a) => Estimated a =
    Estimated { truths    :: [Double]
              , estimates :: [Double]
              , samples   :: [a]
              , trace :: Trace
              }
    deriving (Show)

-- | Returns (element from given index, rest).
takeBut :: [a] -> Int -> (a,[a])
takeBut xs index = if index < length xs
                        then (xs !! index, take index xs ++ drop (index+1) xs)
                        else error "takeBut: index too big."

fitnessAvg :: [Double] -> [Double] -> Double
fitnessAvg truths estimates = avg $ zipWith (\a b -> abs (a-b)) truths estimates

calcFitness = fitnessAvg
