module RegressorTests where

import Control.Monad
import Guesswork
import Guesswork.Math.Statistics
import System.Exit (exitFailure)

import TestSetup
import Text.Printf


assertLists xs ys =
    if xs == ys
        then return ()
        else do
            putStrLn "Lists not same!"
            let f = mapM_ (putStr . printf "%4.3f ") 
            f xs
            putStrLn "!="
            f ys
            exitFailure

-- No DRY, very wet


-- | This test tests a property that states:
-- train only + estimate manually == train & test automatically
-- Note that to pass this test the same scaling transform must
-- also be done in both cases.
knnManualEstimationWithScalingEqualsAutomatic = do
    train <- readFeatureFile trainFile
    test  <- readFeatureFile testFile
    let arr  = alreadySeparated train test
    set1 <- fmap estimates . runGuesswork $ arr >>= scale >>= kNN
    t <- runGuesswork $ onlyTrain train >>= scale >>= trainKNN
    let set2 = map (guessWith t . features) test
    assertLists set1 set2

-- | This test tests a property that states:
-- train only + estimate manually == train & test automatically
-- Note that to pass this test the same scaling transform must
-- also be done in both cases.
linearManualEstimationWithScalingEqualsAutomatic = do
    train <- readFeatureFile trainFile
    test  <- readFeatureFile testFile
    let arr  = alreadySeparated train test
    set1 <- fmap estimates . runGuesswork $ arr >>= scale >>= linear
    t <- runGuesswork $ onlyTrain train >>= scale >>= trainLinear
    let set2 = map (guessWith t . features) test
    assertLists set1 set2


-- | This test tests a property that states:
-- train only + estimate manually == train & test automatically
-- Note that to pass this test the same scaling transform must
-- also be done in both cases.
svrManualEstimationWithScalingEqualsAutomatic = do
    train <- readFeatureFile trainFile
    test  <- readFeatureFile testFile
    let arr  = alreadySeparated train test
        conf = defaultSVR 
    set1 <- fmap estimates . runGuesswork $ arr >>= scale >>= svr conf
    t <- runGuesswork $ onlyTrain train >>= scale >>= trainSVR conf
    let set2 = map (guessWith t . features) test
    assertLists set1 set2

testKNNLeaveOneOut = do
    train <- readFeatureFile trainFile
    a <- runGuesswork $ leaveOneOut train >>= pass >>= kNN >>= analyze
    print a

testLinearLeaveOneOut = do
    train <- readFeatureFile trainFile
    a <- runGuesswork $ leaveOneOut train >>= pass >>= linear >>= analyze
    print a

spit :: (Sample a) => [a] -> [Double] -> IO ()
spit set estimates = do 
    print $ sampleCorrelation (map target set) estimates

allRegressorTests = ([
     ( knnManualEstimationWithScalingEqualsAutomatic, "knnManualEstimationWithScalingEqualsAutomatic" )
    ,( linearManualEstimationWithScalingEqualsAutomatic, "linearManualEstimationWithScalingEqualsAutomatic" )
    ,( svrManualEstimationWithScalingEqualsAutomatic, "svrManualEstimationWithScalingEqualsAutomatic" )
    ,( testKNNLeaveOneOut, "knn leave-one-out" )
    ,( testLinearLeaveOneOut, "linear leave-one-out" )
   ],"Regressor tests")
