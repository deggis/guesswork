{-# LANGUAGE RecordWildCards #-}
module Guesswork.Flow where

import Control.Monad.RWS.Strict
import qualified Data.Vector.Unboxed as VU
import qualified Guesswork.Arrange as ARRANGE
import qualified Guesswork.Transform as TRANSFORM
import qualified Guesswork.Estimate as ESTIMATE
import qualified Guesswork.Analyze as ANALYZE
import qualified Guesswork.Estimate.KNN as KNN
import Guesswork.Types

data GuessworkFlow a = EstimateFlow { arrangement :: Guesswork ARRANGE.Arranged
                                  , transformer :: ARRANGE.Arranged -> Guesswork TRANSFORM.Transformed
                                  , estimator :: TRANSFORM.Transformed -> Guesswork (ESTIMATE.Result a)
                                  , analyzer :: ESTIMATE.Result a -> Guesswork ANALYZE.Analyzed }

defaultKnnEstimate :: Guesswork ARRANGE.Arranged -> GuessworkFlow KNN.KNN
defaultKnnEstimate arrangement =
    let transformer  = TRANSFORM.scale
        estimator    = KNN.knn
        analyzer     = ANALYZE.analyze
    in EstimateFlow{..}

runGuesswork :: Estimator a => GuessworkFlow a -> IO ANALYZE.Analyzed
runGuesswork EstimateFlow{..} = do
    let tool = arrangement >>=
               transformer >>=
               estimator >>=
               analyzer
    (a,_,_) <- runRWST tool defaultConf ()
    return a
