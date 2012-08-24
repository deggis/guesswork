module IOTests where

import Control.Monad
import System.Exit (exitFailure,exitSuccess)
import Guesswork
import Data.Serialize
import qualified Guesswork.Transform as T
import TestSetup


assertSerialization t =
    let
        res = decode . encode $ t
    in case res of
        Left s -> do
            putStrLn s
            exitFailure
        Right r ->
            if r == t
                then putStrLn "Ok."
                else exitFailure
        

testIOReadFeatureFiles = do
    train <- readFeatureFile trainFile
    test  <- readFeatureFile testFile
    when (length (train++test) /= 91) exitFailure

-- Serialize instances

scaleTransformSerialization = do
    train <- readFeatureFile trainFile
    let
        t = T.getTransform (T.Scale T.defaultScaleConfig) . map features $ train
    assertSerialization t

knnSerialization = do
    train <- readFeatureFile trainFile
    t <- runGuesswork $ onlyTrain train >>= scale >>= trainKNN
    assertSerialization t

linearSerialization = do
    train <- readFeatureFile trainFile
    t <- runGuesswork $ onlyTrain train >>= scale >>= trainLinear
    assertSerialization t

svrSerialization = do
    train <- readFeatureFile trainFile
    let conf = defaultSVR
    t <- runGuesswork $ onlyTrain train >>= scale >>= trainSVR conf
    assertSerialization t


ioTests = ([
    (testIOReadFeatureFiles, "testIOReadFeatureFiles")
   ,(scaleTransformSerialization, "scaleTransformSerialization")
   ,(knnSerialization, "knnSerialization")
   ,(linearSerialization, "linearSerialization")
   ],"I/O tests")

