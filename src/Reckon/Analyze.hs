{-# LANGUAGE RecordWildCards #-}
module Reckon.Analyze where

import Reckon.Types
import Reckon.Math.Statistics
import qualified Reckon.Estimate as ESTIMATE

data Analyzed = Analyzed { trace :: Trace
                         , minError :: Double
                         , maxError :: Double
                         , avgError :: Double
                         , rmse :: Double }
    deriving Show

analyze :: ESTIMATE.Estimated  -> Reckon Analyzed
analyze ESTIMATE.Estimated{..} =
    let errors   = map abs $ zipWith (-) truths estimates
        minError = minimum errors
        maxError = maximum errors
        avgError = avg errors
        rmse     = calcRMSE truths estimates
    in return $ Analyzed{..}

