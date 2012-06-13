module Main where

import Guesswork
import Control.Monad.RWS.Strict

main = do
    train <- readFeatureFile "data1_train.scaled"
    test  <- readFeatureFile "data1_test.scaled"
    let arr  = alreadySeparated train test
        flow = arr >>= scale >>= knn >>= analyze
    (a,_,_) <- runRWST flow defaultConf ()
    print a
