{-# LANGUAGE RecordWildCards #-}
module Guesswork.Analyze where

import Guesswork.Types
import Guesswork.Math.Statistics
import qualified Guesswork.Estimate as ESTIMATE

data Analyzed = Analyzed { trace :: Trace
                         , minError :: Double
                         , maxError :: Double
                         , avgError :: Double
                         , rmse :: Double }
    deriving Show

analyze :: ESTIMATE.Result a -> Guesswork Analyzed
analyze ESTIMATE.Estimates{..} =
    let errors   = map abs $ zipWith (-) truths estimates
        minError = minimum errors
        maxError = maximum errors
        avgError = avg errors
        rmse     = calcRMSE truths estimates
    in return $ Analyzed{..}

