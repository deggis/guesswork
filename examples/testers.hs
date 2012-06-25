module Main where

import Guesswork

-- short examples demonstrating different (simple) use cases

mainKNN = do
    train <- readFeatureFile "data1_train.scaled"
    test  <- readFeatureFile "data1_test.scaled"
    let arr  = alreadySeparated train test
    a <- runGuesswork $ arr >>= scale >>= kNN >>= analyze
    print a

mainKNNTrain = do
    train <- readFeatureFile "data1_train.scaled"
    test  <- readFeatureFile "data1_test.scaled"
    t <- runGuesswork $ onlyTrain train >>= scale >>= trainKNN
    print $ map (guessWith t . snd) test

mainKNNLeaveOneOut = do
    train <- readFeatureFile "data1_train.scaled"
    a <- runGuesswork $ leaveOneOut train >>= pass >>= kNN >>= analyze
    print a

mainLinear = do
    train <- readFeatureFile "data1_train.scaled"
    test  <- readFeatureFile "data1_test.scaled"
    let arr  = alreadySeparated train test
    a <- runGuesswork $ arr >>= pass >>= linear >>= analyze
    print a

mainLinearTrain = do
    train <- readFeatureFile "data1_train.scaled"
    test  <- readFeatureFile "data1_test.scaled"
    t <- runGuesswork $ onlyTrain train >>= pass >>= trainLinear
    print $ map (guessWith t . snd) test

mainLinearLeaveOneOut = do
    train <- readFeatureFile "data1_train.scaled"
    a <- runGuesswork $ leaveOneOut train >>= pass >>= linear >>= analyze
    print a

main = mainKNN
    >> mainKNNTrain
    >> mainKNNLeaveOneOut
    >> mainLinear
    >> mainLinearTrain
    >> mainLinearLeaveOneOut
