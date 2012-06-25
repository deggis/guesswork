{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Guesswork.Estimate where

import Control.Arrow
import Data.Ord
import Data.List
import Guesswork.Types
import Guesswork.Math.Statistics
import qualified Guesswork.Transform as TRANSFORM

data Estimated = Estimated { truths    :: [Double]
                           , estimates :: [Double]
                           , trace :: Trace
                           }
