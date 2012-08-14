{-# LANGUAGE RecordWildCards #-}
module Guesswork.Analyze where

import Guesswork.Types
import Guesswork.Math.Statistics as S
import qualified Guesswork.Estimate as ESTIMATE

data Analyzed = Analyzed { trace :: Trace
                         , minError :: Double
                         , maxError :: Double
                         , avgError :: Double
                         , rmse :: Double }
    deriving Show

analyze :: ESTIMATE.Estimated -> Guesswork Analyzed
analyze ESTIMATE.Estimated{..} =
    let errors   = map abs $ zipWith (-) truths estimates
        minError = minimum errors
        maxError = maximum errors
        avgError = avg errors
        rmse     = S.rmse truths estimates
    in return $ Analyzed{..}

