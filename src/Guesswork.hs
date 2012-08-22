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
    , runGuesswork

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
    , scale'
    , defaultScaleConfig
    , pass

      -- ** Estimation
    , GuessworkEstimator
    , guessWith
    , kNN
    , kNN'
    , KNNConfig(..)
    , defaultKNN
    , trainKNN
    , trainKNN'
    , linear
    , trainLinear
    , svr 
    , defaultSVR
    , SVRConfig(..)
    , Estimated(..)

      -- ** Result analyzing
    , analyze

    ) where

import Guesswork.Types
import Guesswork.IO
import Guesswork.Estimate
import Guesswork.Estimate.KNN
import Guesswork.Estimate.Linear
import Guesswork.Estimate.SVR
import Guesswork.Arrange
import Guesswork.Transform
import Guesswork.Analyze
import Guesswork.Flow
