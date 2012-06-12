module Main where

import Guesswork

main = do
    train <- readFeatureFile "data1_train.scaled"
    test  <- readFeatureFile "data1_test.scaled"
    let arrangement = alreadySeparated train test
        flow        = (defaultKnnEstimate arrangement) { transformer = pass }
    r <- runGuesswork flow
    print r
