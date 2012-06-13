module Main where

import Control.Monad.RWS.Strict
import Control.Monad
import Guesswork

main = do
    train <- readFeatureFile "data1_train.scaled"
    test  <- readFeatureFile "data1_test.scaled"
    let flow = onlyTrain train >>= scale >>= knn >>= function
    (t,_,_) <- runRWST flow defaultConf ()
    print $ map (estimate t . snd) test
