{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Guesswork.Estimate.KNN where

import Data.List
import Data.Ord
import Control.Arrow
import System.IO.Unsafe

import Guesswork.Types
import qualified Guesswork.Transform as TRANSFORM


