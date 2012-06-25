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

data Linear = Linear (PM.Matrix Double) TRANSFORM.Operation Trace

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
linear (TRANSFORM.LeaveOneOut samples trace) = error "loo not supported yet"

trainLinear :: TRANSFORM.Transformed -> Guesswork Linear
trainLinear (TRANSFORM.OnlyTrain train op trace) = do
    let trainFeatures = featureMatrix train
        trainTruth    = PV.fromList . map fst $ train
        solution      = solve trainFeatures trainTruth 
        trace'        = trace ++ ",R=linear"
    return $ Linear solution op trace'

solve trainFeatures trainTruth = Algo.linearSolveSVD trainFeatures $ PM.fromColumns [trainTruth]

featureMatrix = PM.fromRows . map (packVector . snd)

apply :: PM.Matrix Double -> V.Vector Double -> Double
apply solution features =
    let features'  = PM.fromRows [packVector features]
        [estimate] = PV.toList . head . PM.toColumns $ features' `N.mXm` solution
    in estimate

-- | Packs given list to Vector and adds slack(?) variable 1 for determining constant C.
packVector :: V.Vector Double -> PV.Vector Double
packVector featureV = PV.fromList $ (V.toList featureV) ++[1]
