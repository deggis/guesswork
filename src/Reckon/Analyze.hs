module Reckon.Analyze where

data Analyzed = Analyzed { trace :: Trace
                         , minError :: Double
                         , maxError :: Double
                         , avgError :: Double
                         , rmse :: Double
                         , nrmse :: Double }
    deriving Show

