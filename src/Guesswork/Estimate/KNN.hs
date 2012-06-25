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
import qualified Debug.Trace as T

data KNN = KNN [Sample] Int TRANSFORM.Operation Trace

instance Estimator KNN where
    estimate (KNN train k op _) = kNNEstimate train k . TRANSFORM.apply op

type KNNFitness = [Sample] -> Int -> Double

data KNNConfig = KNNConfig { ks :: [Int]
                           , fitness :: KNNFitness }

-- | Default KNN configuration: test k values between 1 and 20.
defaultKNN = KNNConfig [1..20] splitFitness

-- | Perform KNN with defaultKNN configuration.
kNN :: TRANSFORM.Transformed -> Guesswork Estimated
kNN t@TRANSFORM.Separated{..}   = kNN' defaultKNN t
kNN t@TRANSFORM.LeaveOneOut{..} = kNN' (defaultKNN { fitness = leaveOneOutFitness }) t

-- | Assumes scaled & prepared data.
kNN' :: KNNConfig -> TRANSFORM.Transformed -> Guesswork Estimated
kNN' conf (TRANSFORM.Separated train test trace) = do
    let bestK     = findBestK conf train
        estimates = map (kNNEstimate train bestK . snd) test
        truths    = map fst test
    return $ Estimated truths estimates (trace ++ ",R=kNN")
kNN' conf (TRANSFORM.LeaveOneOut samples trace) = do
    let bestK     = findBestK conf samples
        indices   = [0..(length samples - 1)]
        pairs     = map (takeBut samples) indices
        estimates = map (\(x,xs) -> kNNEstimate xs bestK (snd x)) pairs
        truths    = map fst samples
    return $ Estimated truths estimates (trace ++ ",R=kNN("++show bestK++")")

trainKNN = trainKNN' defaultKNN

trainKNN':: KNNConfig -> TRANSFORM.Transformed -> Guesswork KNN
trainKNN' conf (TRANSFORM.OnlyTrain samples op trace) = do
    let bestK  = findBestK conf samples
    return $ KNN samples bestK op (trace ++ ",R=kNN")
trainKNN' _ _ = error "Unsupported knn operation."

findBestK :: KNNConfig -> [Sample] -> Int
findBestK KNNConfig{..} samples =
    let paired = map (\k->(k, fitness samples k)) ks
        bestK = fst . head . sortBy (comparing snd) $ paired
    in bestK

splitFitness :: KNNFitness
splitFitness samples k =
    let (train,test) = splitAt (length samples `div` 2) samples
        estimates = map (kNNEstimate train k . snd) test
        truths    = map fst test
    in calcFitness truths estimates

leaveOneOutFitness :: KNNFitness
leaveOneOutFitness samples k = 
    let indices   = [0..(length samples - 1)]
        pairs     = map (takeBut samples) indices
        estimates = map (\(x,xs) -> kNNEstimate xs k (snd x)) pairs
        truths    = map fst samples
    in calcFitness truths estimates

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
