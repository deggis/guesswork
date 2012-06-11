module Guesswork.Transform.Scale where

import qualified Data.Vector.Unboxed as VU
import qualified Data.Packed.Vector as PV
import qualified Data.Packed.Matrix as PM
import Data.List
import Guesswork.Types

type FeatureValues = VU.Vector Double

toPacked = PV.fromList . VU.toList
fromPacked = VU.fromList . PV.toList

rotateVecs :: [FeatureVector] -> [FeatureValues]
rotateVecs [] = []
rotateVecs vecs  =
    let packed = map toPacked vecs
        mat    = PM.fromRows packed
        cols   = PM.toColumns mat
    in map fromPacked cols

type Ranges = [(Double,Double)]

-- | Vectors -> (min,max) for each attribute (vector index)
calcRanges :: [FeatureVector] -> Ranges
calcRanges = map (\v -> (VU.minimum v, VU.maximum v)) . rotateVecs

-- | Scales a vector using vector index-vice (min,max) ranges.
scaleUsingRanges :: FeatureVector -> Ranges -> FeatureVector
scaleUsingRanges vec ranges = VU.fromList . map (uncurry scaleValue) . zip ranges . VU.toList $ vec

epsilon  = 0.0000000001

-- | Simply scales value. If value range length is < epsilon, scaling results zero.
scaleValue :: (Double,Double) -> Double -> Double
scaleValue (min',max') v = 
    let l        = max' - min' -- range length
     in if (abs l > epsilon)
            then (v - min') / l
            else 0

{-
-- | Tests that scaled values are between 0 and 1
-- FIXME: generation produces rarely cases that can be testd.
prop_scaled_values_between_range lists =
    all (not.null) lists && equalLengths ==> all (\v -> minimum v >= (0-epsilon) && maximum v <= (1+epsilon)) rotated
  where
    equalLengths = 1 == (length . group . map length $ lists)
    scaled       = scaleUsingRanges lists $ calcRanges lists
    rotated      = rotate2DArray scaled

-}
