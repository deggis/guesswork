{-# LANGUAGE RecordWildCards #-}

{-|

This module contains basic Guesswork definitions for reading/writing
files and basic tools to get started quickly.

-}
module Guesswork
    (
      -- * Guesswork Monad
      Guesswork
    , Conf
    , defaultConf

      -- ** Guesswork flows
    , defaultKnnEstimate
    , runGuesswork
    , GuessworkFlow(..)

      -- ** Essential types
    , Sample
    , FeatureVector

      -- ** IO: Reading/writing files
    , readFeatureFile
    , writeFeatureFile

      -- ** Arrange data
    , splitWithRatio
    , leaveOneOut
    , alreadySeparated
    , onlyTrain

      -- ** Transform data
    , scale
    , pass

      -- ** Estimation
    , Estimator
    , estimate
    , function
    , knn

      -- ** Result analyzing
    , analyze

    ) where

import Guesswork.Types
import Guesswork.IO
import Guesswork.Estimate
import Guesswork.Estimate.KNN
import Guesswork.Arrange
import Guesswork.Transform
import Guesswork.Analyze
import Guesswork.Flow