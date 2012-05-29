module AI.UASI.Utils.Misc where

import Data.List

-- | 'Rotates' 2D array, columns to rows
-- Most likely inefficient as a dead horse.
rotate2DArray :: [[Double]] -> [[Double]]
rotate2DArray [] = []
rotate2DArray d  =
    let nrows = length $ head d -- number of new rows
        ncols = length d -- number of new cols
        row :: Int -> [Double]
        row c = map (!!(c-1)) d
     in map row [1..nrows]

-- | Vectors -> (min,max) for each attribute (vector index)
calcRanges :: [[Double]] -> [(Double,Double)]
calcRanges datas = map (\v -> (minimum v, maximum v)) $ rotate2DArray datas

-- | Scales a vector using vector index-vice (min,max) ranges.
scaleUsingRanges :: [Double] -> [(Double,Double)] -> [Double]
scaleUsingRanges vec ranges = map (\(v,range) -> scaleValue v range) $ zip vec ranges

epsilon  = 0.0000000001

-- | Simply scales value. If value range length is < epsilon, scaling results zero.
scaleValue :: Double -> (Double,Double) -> Double
scaleValue v (min',max') = 
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
