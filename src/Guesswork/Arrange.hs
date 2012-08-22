{-# LANGUAGE RecordWildCards #-}
module Guesswork.Arrange where

import Guesswork.Types
import Control.Monad
import Control.Arrow
import Control.Exception
import qualified Data.Vector.Unboxed as V
import Math.KMeans
import Guesswork.Transform.Scale

data Arranged = Separated { train :: [Sample]
                          , test :: [Sample]
                          , trace :: Trace }
              | LeaveOneOut [Sample] Trace
              | OnlyTrain [Sample] Trace
    deriving(Show)

-- |Splits data to training and testing using given ratio
-- 'trainAmount' that is used for training, 0.5 as 50 %.
-- First samples are used for training.
splitWithRatio :: Double -> [Sample] -> Guesswork Arranged
splitWithRatio trainAmount samples = do
    let n          = length samples
        toTraining = floor $ fromIntegral n * trainAmount
        train      = take toTraining samples
        test       = drop toTraining samples
        trace      = "A=split"
    when (train == []) . throw $ ArrangeException "Arranging failed: train set empty!"
    when (test  == []) . throw $ ArrangeException "Arranging failed: test set empty!"
    return Separated{..}

-- |Use data as leave-one-out
leaveOneOut :: [Sample] -> Guesswork Arranged
leaveOneOut samples = do
    when (samples == []) . throw $ ArrangeException "Empty sample set!"
    let trace = "A=leaveoneout"
    return $ LeaveOneOut samples trace

-- |Use beforehand separated train & test data.
alreadySeparated :: [Sample] -> [Sample] -> Guesswork Arranged
alreadySeparated train test = do
    let trace = "A=preseparated"
    return Separated{..}

onlyTrain :: [Sample] -> Guesswork Arranged
onlyTrain samples = do
    let trace = "A=onlytrain"
    return $ OnlyTrain samples trace

-- | Separates samples with kmeans.
-- For k-means, the features are used as scaled [0,1].
-- Returned sample sets are still in their original state.
-- Using fixed ratio of 2:1 for train:test.
-- FIXME: enable splitting with different ratios
separateDataKMeans :: Int -> [Sample] -> Guesswork Arranged
separateDataKMeans nClusters samples = do
    let
        ranges      = calcRanges . map snd $ samples
        scaledPairs = map (\s -> (scaleUsingRanges (0,1) ranges . snd $ s, s)) samples 
        -- Create clusters
        clusters    = kmeans nClusters scaledPairs
        -- Flatten clusters and pick data using 1:2.
        ordered     = zip (cycle [1..3]) . map snd . concat $ clusters
        picker p    = map snd . filter (\(i,_) -> p i) $ ordered
        train       = picker (<3)
        test        = picker (==3)
        trace       = "A=kmeans"
    return Separated{..}

