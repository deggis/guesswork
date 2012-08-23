module IOTests where

import Control.Monad
import System.Exit (exitFailure,exitSuccess)
import Guesswork

import TestSetup

testIOReadFeatureFiles = do
    train <- readFeatureFile trainFile
    test  <- readFeatureFile testFile
    when (length (train++test) /= 91) exitFailure

ioTests = ([
    (testIOReadFeatureFiles, "testIOReadFeatureFiles")
   ],"I/O tests")

