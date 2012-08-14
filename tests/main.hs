module Main where

import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Monadic
import Text.Printf
import qualified Guesswork.Math.Statistics as S
import Debug.Trace

main = mapM_ (\(s,a) -> printf "%-25s" s >> a) tests

eps = 0.000000001

prop_avg :: [Double] -> Property
prop_avg xs = length xs > 0 ==>
    let
        n = fromIntegral . length $ xs
        s = sum xs
        avg' = s/n
    in property $ abs (avg' - S.avg xs) < eps

listDiff xs ys = sum $ zipWith (\x y -> abs (x-y)) xs ys

genLists n = do
    xs <- vector n :: Gen [Double]
    ys <- vector n :: Gen [Double]
    return (xs,ys)

prop_correlation = do
    n <- choose (2,100)
    (xs,ys) <- genLists n
    let
        corr = S.sampleCorrelation xs ys
    return . property $ corr >= (-1-eps) && corr <= (1+eps)

tests = [("Statistics.avg", quickCheck prop_avg)
        ,("Statistics.correlation", quickCheck prop_correlation)
        ]
