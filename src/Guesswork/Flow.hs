{-# LANGUAGE RecordWildCards #-}
module Guesswork.Flow where

import Control.Monad.RWS.Strict
import qualified Data.Vector.Unboxed as VU
import qualified Guesswork.Arrange as ARRANGE
import qualified Guesswork.Transform as TRANSFORM
import qualified Guesswork.Estimate as ESTIMATE
import qualified Guesswork.Analyze as ANALYZE
import Guesswork.Types

data GuessworkFlow = EstimateFlow { arrangement :: Guesswork ARRANGE.Arranged
                                  , transformer :: TRANSFORM.Transformer
                                  , estimator :: ESTIMATE.Estimator
                                  , analyzer :: ANALYZE.Analyzer }

defaultKnnEstimate :: Guesswork ARRANGE.Arranged -> GuessworkFlow
defaultKnnEstimate arrangement =
    let transformer  = TRANSFORM.scale
        estimator    = ESTIMATE.knn
        analyzer     = ANALYZE.analyze
    in EstimateFlow{..}

runGuesswork :: GuessworkFlow -> IO ANALYZE.Analyzed
runGuesswork EstimateFlow{..} = do
    let tool = arrangement >>=
               transformer >>=
               estimator >>=
               analyzer
    (a,_,_) <- runRWST tool defaultConf ()
    return a
