{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Guesswork.Estimate.KNN where

import Data.List
import Data.Ord
import Data.Serialize
import Control.Arrow
import System.IO.Unsafe

--import GHC.Generics

import Guesswork.Types
import Guesswork.Math.Statistics
import qualified Guesswork.Transform as TRANSFORM
import Guesswork.Estimate
import qualified Debug.Trace as T

-- | KNN data type represents KNN estimator for type Sample a.
-- Serializing KNN requires also serializable Sample a type,
-- since KNN is merely the samples and the transform operation.
data (Sample a, Serialize a) => KNN a =
    KNN [a] Int TRANSFORM.Operation Trace
    deriving (Eq)


instance (Serialize a, Sample a) => Serialize (KNN a) where
    get = do
        samples <- get
        k <- get
        op <- get
        t <- get
        return $ KNN samples k op t
        
    put (KNN samples k op t) = do
        put samples
        put k
        put op
        put t


instance (Sample a, Serialize a) => GuessworkEstimator (KNN a) where
    guessWith (KNN samples k op _) = kNNEstimate samples k . TRANSFORM.apply op

    


type KNNFitness = (Sample a) => [a] -> Int -> Double

data KNNConfig = KNNConfig { ks :: [Int]
                           , fitness :: KNNFitness
                           }

-- | Default KNN configuration: test k values between 1 and 20.
defaultKNN = KNNConfig [1..20] splitFitness

-- | Perform KNN with defaultKNN configuration.
kNN :: (Sample a) => TRANSFORM.Transformed a -> Guesswork (Estimated a)
kNN t@TRANSFORM.Separated{..}   = kNN' defaultKNN t
kNN t@TRANSFORM.LeaveOneOut{..} = kNN' defaultKNN t

-- | Assumes scaled & prepared data.
kNN' :: (Sample a) => KNNConfig -> TRANSFORM.Transformed a -> Guesswork (Estimated a)
kNN' conf (TRANSFORM.Separated train test trace) = do
    let
        bestK     = findBestK conf train
        estimates = map (kNNEstimate train bestK . features) test
        truths    = map target test
    return $ Estimated truths estimates test (trace ++ ",R=kNN("++show bestK++")")
kNN' conf (TRANSFORM.LeaveOneOut samples trace) = do
    let
        bestK     = findBestK conf samples
        indices   = [0..(length samples - 1)]
        pairs     = map (takeBut samples) indices
        estimates = map (\(x,xs) -> kNNEstimate xs bestK (features x)) pairs
        truths    = map target samples
    return $ Estimated truths estimates samples (trace ++ ",R=kNN("++show bestK++")")

trainKNN = trainKNN' defaultKNN

trainKNN':: (Sample a, Serialize a) => KNNConfig -> TRANSFORM.Transformed a -> Guesswork (KNN a)
trainKNN' conf (TRANSFORM.OnlyTrain samples op trace) = do
    let bestK  = findBestK conf samples
    return $ KNN samples bestK op (trace ++ ",R=kNN")
trainKNN' _ _ = error "Unsupported knn operation."

findBestK :: (Sample a) => KNNConfig -> [a] -> Int
findBestK KNNConfig{..} samples =
    let paired = map (\k->(k, fitness samples k)) ks
        bestK = fst . head . sortBy (comparing snd) $ paired
    in bestK

-- FIXME: generalize
splitFitness :: KNNFitness
splitFitness samples k =
    let (train,test) = splitAt (length samples `div` 2) samples
        estimates = map (kNNEstimate train k . features) test
        truths    = map target test
    in calcFitness truths estimates

-- FIXME: generalize
leaveOneOutFitness :: KNNFitness
leaveOneOutFitness samples k = 
    let indices   = [0..(length samples - 1)]
        pairs     = map (takeBut samples) indices
        estimates = map (\(x,xs) -> kNNEstimate xs k (features x)) pairs
        truths    = map target samples
    in calcFitness truths estimates

kNNEstimate :: (Sample a) => [a] -> Int -> FeatureVector -> Double
kNNEstimate others k vec =
    let f x         = (x, euclidianNorm vec (features x))
        byDistances = sortBy (comparing snd) . map f $ others
        kNearest    = map target . take k . map fst $ byDistances
    in avg kNearest

flip' (a,b) = (b,a)


