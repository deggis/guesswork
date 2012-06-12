{-# LANGUAGE RecordWildCards #-}
module Guesswork.Flow where

import Control.Monad.RWS.Strict
import qualified Data.Vector.Unboxed as VU
import qualified Guesswork.Arrange as ARRANGE
import qualified Guesswork.Transform as TRANSFORM
import qualified Guesswork.Estimate as ESTIMATE
import qualified Guesswork.Analyze as ANALYZE
import Guesswork.Types

data GuessworkFlow = EstimateFlow { arranger :: ARRANGE.Arranger
                                  , transformer :: TRANSFORM.Transformer
                                  , estimator :: ESTIMATE.Estimator
                                  , analyzer :: ANALYZE.Analyzer }

defaultKnnEstimate :: GuessworkFlow
defaultKnnEstimate =
    let arranger    = ARRANGE.splitWithRatio 0.5
        transformer = TRANSFORM.scale
        estimator   = ESTIMATE.knn
        analyzer    = ANALYZE.analyze
    in EstimateFlow{..}

runGuesswork :: [Sample] -> GuessworkFlow -> IO ANALYZE.Analyzed
runGuesswork samples EstimateFlow{..} = do
    let tool = arranger samples >>=
               transformer >>=
               estimator >>=
               analyzer
    (a,_,_) <- runRWST tool defaultConf ()
    return a
