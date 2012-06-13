{-# LANGUAGE RecordWildCards #-}
module Guesswork.Arrange where

import Guesswork.Types
import Control.Monad
import Control.Exception
import qualified Data.Vector.Unboxed as V

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
