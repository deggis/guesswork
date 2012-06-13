{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Guesswork.Estimate where

import Control.Arrow
import Data.Ord
import Data.List
import Guesswork.Types
import Guesswork.Math.Statistics
import qualified Guesswork.Transform as TRANSFORM

data Result a = Estimates { truths    :: [Double]
                         , estimates :: [Double]
                         , trace :: Trace
                         }
             | Worker    { estimator :: a}
    deriving Show

function :: Estimator a => Result a -> Guesswork a
function (Worker a) = return a
function _ = error "Ei tullu suksia."

