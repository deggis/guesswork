module Guesswork.Math.Statistics where

import qualified Data.Vector.Unboxed as VU
import Guesswork.Types

kh xs = sqrt . (/n) . sum . map (\x->(x-avg')**2) $ xs
  where avg' = avg xs
        n    = fromIntegral . length $ xs

avg [] = error "Empty list!"
avg xs = sum xs / (fromIntegral . length $ xs)

euclidianNorm :: FeatureVector -> FeatureVector -> Double
euclidianNorm u v = VU.sum $ VU.zipWith (\a b -> (a - b)**2) u v

-- |Scales lists (truths, predictions) between [0,1] using
-- scales from truths.
-- NOTE: this means that all predictions aren't necessarily
-- between [0,1].
scaleLists truths preds =
    let minT  = minimum truths
        maxT  = maximum truths
        scale = map ((/ maxT) . subtract minT)
    in (scale truths, scale preds)

-- |Calculate population variance.
popVariance :: [Double] -> Double
popVariance xs =
  let n  = fromIntegral . length $ xs
      ax = avg xs
  in (*(1/n)) . sum . map ((**2) . subtract ax) $ xs

-- |Pearson's sample correlation. Correlation is in range
-- [-1,1] and is 1.0 if lists are identical.
-- List aren't checked to be of same length.
sampleCorrelation :: [Double] -> [Double] -> Double
sampleCorrelation xs ys 
    | length xs < 2 || length ys < 2 = error "Correlation: too short lists." 
    | otherwise                      = if (xs == ys) then 1.0 else corr
  where
    avgX   = avg xs
    avgY   = avg ys
    nxs    = map (subtract avgX) xs
    nys    = map (subtract avgY) ys
    upper  = sum $ zipWith (*) nxs nys
    lowerX = sum $ map (**2) nxs
    lowerY = sum $ map (**2) nys
    lower  = sqrt $ lowerX * lowerY
    corr   = upper / lower

-- |Calculate root mean square error from given list of
-- truths and predictions.
rmse :: [Double] -> [Double] -> Double
rmse truths preds =
    let errors = sum . map (**2) $ zipWith (-) truths preds
        n      = fromIntegral . length $ truths
    in sqrt $ errors / n
