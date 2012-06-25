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

instance Estimator Linear where
    estimate (Linear solution op _) = apply solution . TRANSFORM.apply op

linear :: TRANSFORM.Transformed -> Guesswork Estimated
linear (TRANSFORM.Separated train test trace) = do
    let trainFeatures = featureMatrix train
        trainTruth    = PV.fromList . map fst $ train
        solution      = solve trainFeatures trainTruth 
        estimates     = map (apply solution . snd) test
        truths        = map fst test
    return $ Estimated truths estimates (trace ++ ",R=linear")
  where
linear (TRANSFORM.LeaveOneOut samples trace) = do
    let indices    = [0..(length samples - 1)]
        groups     = map (takeBut samples) indices
        solutions  = map (second trainLinear') groups
        truths     = map fst samples
        estimates  = map (uncurry apply . second snd . flip') solutions
    return $ Estimated truths estimates (trace ++ ",R=linear")
  where
    flip' (a,b) = (b,a)

trainLinear :: TRANSFORM.Transformed -> Guesswork Linear
trainLinear (TRANSFORM.OnlyTrain train op trace) = do
    let solution = trainLinear' train
    return $ Linear solution op (trace ++ ",R=linear")

trainLinear' :: [Sample] -> Solution
trainLinear' train =
    let trainFeatures = featureMatrix train
        trainTruth    = PV.fromList . map fst $ train
    in solve trainFeatures trainTruth

solve trainFeatures trainTruth = Algo.linearSolveSVD trainFeatures $ PM.fromColumns [trainTruth]

featureMatrix = PM.fromRows . map (packVector . snd)

apply :: Solution -> V.Vector Double -> Double
apply solution features =
    let features'  = PM.fromRows [packVector features]
        [estimate] = PV.toList . head . PM.toColumns $ features' `N.mXm` solution
    in estimate

-- | Packs given list to Vector and adds slack(?) variable 1 for determining constant C.
packVector :: V.Vector Double -> PV.Vector Double
packVector featureV = PV.fromList $ (V.toList featureV) ++[1]
