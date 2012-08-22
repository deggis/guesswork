{-# LANGUAGE RecordWildCards #-}
module Guesswork.Analyze where

import Guesswork.Types
import Guesswork.Math.Statistics as S
import qualified Guesswork.Estimate as ESTIMATE

data (Sample a) => Analyzed a =
    Analyzed { trace :: Trace
             , minError :: Double
             , maxError :: Double
             , avgError :: Double
             , rmse :: Double
             , estimations :: ESTIMATE.Estimated a
             }
    deriving Show

analyze :: (Sample a) => ESTIMATE.Estimated a -> Guesswork (Analyzed a)
analyze e@ESTIMATE.Estimated{..} =
    let errors   = map abs $ zipWith (-) truths estimates
        minError = minimum errors
        maxError = maximum errors
        avgError = avg errors
        rmse     = S.rmse truths estimates
        estimations = e
    in return $ Analyzed{..}

