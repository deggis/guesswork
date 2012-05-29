{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reckon.Estimation where

import Reckon
import Reckon.Estimation.KNN

data EstimationResults = EstimationResults { truth :: [Double]
                                           , predicted :: [Double]
                                           , message :: String
                                           , setupScores_ :: [String] }

type Estimator = Transformed -> Reckon Result

data Result = Result { truths    :: [Double]
                     , estimates :: [Double]
                     , trace :: Trace
                     }
    deriving Show

extractAttribute KnownPlot{..} = kok_massa
extractAttribute ReferencePlot{..} = kok_massa 
extractAttributes = map (extractAttribute . fst)
type Estimator = [JuicyPack] -> [JuicyPack] -> EstimationResults

fitnessAvg :: [Double] -> [Double] -> Double
fitnessAvg truths estimates = avg $ zipWith (\a b -> abs (a-b)) truths estimates
