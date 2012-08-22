{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Guesswork.Estimate.Linear where

import Data.List
import Data.Ord
import Control.Arrow
import qualified Data.Vector.Unboxed as V
import qualified Data.Packed.Matrix as PM
import qualified Data.Packed.Vector as PV
import qualified Numeric.LinearAlgebra.Algorithms as Algo
import qualified Numeric.Container as N

import Guesswork.Types
import Guesswork.Math.Statistics
import qualified Guesswork.Transform as TRANSFORM
import Guesswork.Estimate

data Linear = Linear Solution TRANSFORM.Operation Trace

type Solution = PM.Matrix Double

instance GuessworkEstimator Linear where
    guessWith (Linear solution op _) = apply solution . TRANSFORM.apply op

linear :: (Sample a) => TRANSFORM.Transformed a -> Guesswork (Estimated a)
linear (TRANSFORM.Separated train test trace) = do
    let
        trainFeatures = featureMatrix train
        trainTruth    = PV.fromList . map target $ train
        solution      = solve trainFeatures trainTruth 
        estimates     = map (apply solution . features) test
        truths        = map target test
        samples       = test
    return $ Estimated truths estimates samples (trace ++ ",R=linear")
  where
linear (TRANSFORM.LeaveOneOut samples trace) = do
    let
        indices    = [0..(length samples - 1)]
        groups     = map (takeBut samples) indices
        solutions  = map (second trainLinear') groups
        truths     = map target samples
        estimates  = map (uncurry apply . second features . flip') solutions
    return $ Estimated truths estimates samples (trace ++ ",R=linear")
  where
    flip' (a,b) = (b,a)

trainLinear :: (Sample a) => TRANSFORM.Transformed a -> Guesswork Linear
trainLinear (TRANSFORM.OnlyTrain train op trace) = do
    let solution = trainLinear' train
    return $ Linear solution op (trace ++ ",R=linear")

trainLinear' :: (Sample a) => [a] -> Solution
trainLinear' train =
    let trainFeatures = featureMatrix train
        trainTruth    = PV.fromList . map target $ train
    in solve trainFeatures trainTruth

solve trainFeatures trainTruth = Algo.linearSolveSVD trainFeatures $ PM.fromColumns [trainTruth]

featureMatrix :: (Sample a) => [a] -> PM.Matrix Double
featureMatrix = PM.fromRows . map (packVector . features)

apply :: Solution -> V.Vector Double -> Double
apply solution features =
    let features'  = PM.fromRows [packVector features]
        [estimate] = PV.toList . head . PM.toColumns $ features' `N.mXm` solution
    in estimate

-- | Packs given list to Vector and adds slack(?) variable 1 for determining constant C.
packVector :: V.Vector Double -> PV.Vector Double
packVector featureV = PV.fromList $ (V.toList featureV) ++[1]
