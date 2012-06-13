{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Guesswork.Estimate.KNN where

import Data.List
import Data.Ord
import Control.Arrow
import System.IO.Unsafe

import Guesswork.Types
import Guesswork.Math.Statistics
import qualified Guesswork.Transform as TRANSFORM
import Guesswork.Estimate

data KNN = KNN [Sample] Int TRANSFORM.Operation Trace

instance Estimator KNN where
    estimate (KNN train k op _) = kNNEstimate train k . TRANSFORM.apply op

-- | Assumes scaled & prepared data.
knn :: TRANSFORM.Transformed -> Guesswork (Result KNN)
knn (TRANSFORM.Separated train test trace) = do
    let bestK = findBestK train
    let estimates    = map (kNNEstimate train bestK . snd) test
        truths       = map fst test
    return $ Estimates truths estimates (trace ++ ",R=kNN")
knn (TRANSFORM.OnlyTrain samples op trace) = do
    let bestK  = findBestK samples
    return . Worker $ KNN samples bestK op (trace ++ ",R=kNN")
knn _ = error "Unsupported knn operation."

findBestK :: [Sample] -> Int
findBestK train =
    let (trainxs,fitxs) = splitAt (length train `div` 2) train
        ks              = [1..20] --TODO: read from confs
        paired          = map (\k -> (k, fitnessKNN trainxs fitxs k)) ks
        bestK           = fst . head . sortBy (comparing snd) $ paired
    in bestK

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
