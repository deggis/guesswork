{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Guesswork.Estimate where

import Control.Arrow
import Data.Ord
import Data.List
import Guesswork.Types
import Guesswork.Math.Statistics
import qualified Guesswork.Transform as TRANSFORM

type Estimator = TRANSFORM.Transformed -> Guesswork Estimated

data Estimated = Estimated { truths    :: [Double]
                           , estimates :: [Double]
                           , trace :: Trace
                           }
    deriving Show


--fitnessAvg :: [Double] -> [Double] -> Double
--fitnessAvg truths estimates = avg $ zipWith (\a b -> abs (a-b)) truths estimates
--

-- | Assumes scaled & prepared data.
knn :: TRANSFORM.Transformed -> Guesswork Estimated
knn (TRANSFORM.Separated train test trace) = do
--    info l $ "Estimating " ++ dataSetBranch
    let (trainSet,fitnessSet) = splitAt (length train `div` 2) train
        ks           = [1..20]
        paired       = map (\k -> (k, fitnessKNN trainSet fitnessSet k)) ks
        bestK        = fst . head . sortBy (comparing snd) $ paired
--    debug l $ concat ["Optimal k = ",show bestK, " for set ", dataSetBranch]
    let estimates    = map (kNNEstimate train bestK . snd) test
        truths       = map fst test
        trace'       = trace ++ ",R=kNN"
--    info l "Done."
    return $ Estimated truths estimates trace'
--  where
--    l = "tundra.knn"

fitnessKNN :: [Sample] -> [Sample] -> Int -> Double
fitnessKNN train fitness k =
    let estimates = map (kNNEstimate train k . snd) fitness
        truths    = map fst fitness
    in 1 `seq` calcFitness truths estimates

kNNEstimate :: [Sample] -> Int -> FeatureVector -> Double
kNNEstimate others k features =
    let f           = second (euclidianNorm features)
        byDistances = sortBy (comparing snd) . map f $ others
        kNearest    = take k . map fst $ byDistances
    in avg kNearest

flip' (a,b) = (b,a)

fitnessAvg :: [Double] -> [Double] -> Double
fitnessAvg truths estimates = avg $ zipWith (\a b -> abs (a-b)) truths estimates

calcFitness = fitnessAvg


-- | Returns (element from given index, rest).
takeBut :: [a] -> Int -> (a,[a])
takeBut xs index = if index < length xs
                        then (xs !! index, take index xs ++ drop (index+1) xs)
                        else error "takeBut: index too big."

--crossValidateKNN :: Int -> [JuicyPack] -> Double
--crossValidateKNN k trainset =
--    let indices   = [0..(length trainset - 1)]
--        pairs     = map (takeBut trainset) indices
--        estimates = map (uncurry (kNNEstimate k) . flip') pairs
--        truths    = extractAttributes trainset
--    in 1 `seq` fitness truths estimates
