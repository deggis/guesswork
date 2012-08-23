{-# LANGUAGE RecordWildCards #-}
module Guesswork.Analyze where

import Guesswork.Types
import Guesswork.Math.Statistics as S
import qualified Guesswork.Estimate as ESTIMATE
import Text.Printf

data (Sample a) => Analyzed a =
    Analyzed { trace :: Trace
             , minError :: Double
             , maxError :: Double
             , avgError :: Double
             , rmse :: Double
             , estimations :: ESTIMATE.Estimated a
             }

instance (Sample a) => Show (Analyzed a) where
    show Analyzed{..} = printf "Set: %s: min/max/avg errors: %4.3f %4.3f %4.3f, rmse: %4.3f" trace minError avgError maxError rmse

analyze :: (Sample a) => ESTIMATE.Estimated a -> Guesswork (Analyzed a)
analyze e@ESTIMATE.Estimated{..} =
    let errors   = map abs $ zipWith (-) truths estimates
        minError = minimum errors
        maxError = maximum errors
        avgError = avg errors
        rmse     = S.rmse truths estimates
        estimations = e
    in return $ Analyzed{..}

