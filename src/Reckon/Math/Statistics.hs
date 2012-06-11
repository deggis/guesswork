module Reckon.Math.Statistics where

import qualified Data.Vector.Unboxed as VU
import Reckon.Types

kh xs = sqrt . (/n) . sum . map (\x->(x-avg')**2) $ xs
  where avg' = avg xs
        n    = fromIntegral . length $ xs

avg xs = sum xs / (fromIntegral . length $ xs)

euclidianNorm :: FeatureVector -> FeatureVector -> Double
euclidianNorm u v = VU.sum $ VU.zipWith (\a b -> (a - b)**2) u v

scaleLists truths preds =
    let minT = minimum truths
        maxT = maximum truths
        scale = map (\x->(x-minT)/maxT)
    in ((scale truths),(scale preds))

calcPopVariance :: [Double] -> Double
calcPopVariance xs =
  let n  = fromIntegral . length $ xs
  in (*(1/n)) . sum $ map (\x->(x-(avg xs))**2) xs

calcRMSE :: [Double] -> [Double] -> Double
calcRMSE truths preds =
    let errors = sum . map (**2) $ zipWith (-) truths preds
        n      = fromIntegral . length $ truths
    in sqrt $ errors / n
