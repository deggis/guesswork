module RegressionTests where

import Guesswork
import Guesswork.Math.Statistics

import TestSetup

testKNN = do
    train <- readFeatureFile trainFile
    test  <- readFeatureFile testFile
    let arr  = alreadySeparated train test
    a <- runGuesswork $ arr >>= scale >>= kNN >>= analyze
    print a

testKNNTrain = do
    train <- readFeatureFile trainFile
    test  <- readFeatureFile testFile
    t <- runGuesswork $ onlyTrain train >>= scale >>= trainKNN
    print $ map (guessWith t . features) test

testKNNLeaveOneOut = do
    train <- readFeatureFile trainFile
    a <- runGuesswork $ leaveOneOut train >>= pass >>= kNN >>= analyze
    print a

testLinear = do
    train <- readFeatureFile trainFile
    test  <- readFeatureFile testFile
    let arr  = alreadySeparated train test
    a <- runGuesswork $ arr >>= pass >>= linear >>= analyze
    print a

testLinearTrain = do
    train <- readFeatureFile trainFile
    test  <- readFeatureFile testFile
    t <- runGuesswork $ onlyTrain train >>= pass >>= trainLinear
    let estimates = map (guessWith t . features) test
    spit test estimates

testLinearLeaveOneOut = do
    train <- readFeatureFile trainFile
    a <- runGuesswork $ leaveOneOut train >>= pass >>= linear >>= analyze
    print a

spit :: (Sample a) => [a] -> [Double] -> IO ()
spit set estimates = do 
    print $ sampleCorrelation (map target set) estimates

allRegressionTests = ([
     ( testKNN, "knn" )
    ,( testKNNTrain, "knn only train" )
    ,( testKNNLeaveOneOut, "knn leave-one-out" )
    ,( testLinear, "linearsvd" )
    ,( testLinearTrain, "linear only train" )
    ,( testLinearLeaveOneOut, "linear leave-one-out" )
   ],"Regression tests")
