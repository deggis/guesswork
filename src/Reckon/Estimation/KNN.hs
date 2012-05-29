{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reckon.Estimation.KNN where

import Data.List
import Data.Ord
import Control.Arrow
import System.IO.Unsafe

import Reckon.Flow

hatakeKNN :: Estimator
hatakeKNN scaledTrainSet scaledTestSet =
    let (trainSet,fitnessSet) = splitAt (length scaledTrainSet `div` 2) scaledTrainSet
        ks           = [1..30]
--       paired       = map (\k -> (crossValidateKNN k scaledTrainSet, k)) ks
        paired       = map (\k -> (fitnessKNN k trainSet fitnessSet, k)) ks
        bestK        = snd . head . sortBy (comparing fst) $ paired
        predicted    = map (kNNEstimate bestK scaledTrainSet) scaledTestSet
        truth        = map (extractAttribute . fst) scaledTestSet
        message      = "k-NN estimation done with best k="++show bestK
        setupScores_ = map (\(err,k)->concat [show err, " ", show k]) paired
    in EstimationResults{..}

fitnessKNN :: Int -> [JuicyPack] -> [JuicyPack] -> Double
fitnessKNN k trainSet fitnessSet =
    let estimates = map (kNNEstimate k trainSet) fitnessSet
        truths    = extractAttributes fitnessSet
    in 1 `seq` fitness truths estimates

distance :: [Double] -> [Double] -> Double
distance u v = sum $ zipWith (\a b -> (a - b)**2) u v

kNNEstimate :: Int -> [JuicyPack] -> JuicyPack -> Double
kNNEstimate k others (_,targetFeatures) =
    let byDistances = map (\(p,f) -> (distance targetFeatures f,(p,f))) others
        sortedK     = take k . map snd . sortBy (comparing fst) $ byDistances
    in avg . extractAttributes $ sortedK

flip' (a,b) = (b,a)

-- | Returns (element from given index, rest).
takeBut :: [a] -> Int -> (a,[a])
takeBut xs index = if index < length xs
                        then (xs !! index, take index xs ++ drop (index+1) xs)
                        else error "takeBut: index too big."

crossValidateKNN :: Int -> [JuicyPack] -> Double
crossValidateKNN k trainset =
    let indices   = [0..(length trainset - 1)]
        pairs     = map (takeBut trainset) indices
        estimates = map (uncurry (kNNEstimate k) . flip') pairs
        truths    = extractAttributes trainset
    in 1 `seq` fitness truths estimates

-- Package as Linear Solver thing

