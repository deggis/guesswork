{-# LANGUAGE DeriveDataTypeable #-}
module Guesswork.Types where

import Control.Monad.RWS.Strict
import Control.Exception
import Data.Typeable
import qualified Data.Vector.Unboxed as V

type Guesswork a = RWST Conf Log () IO a

data Conf = Conf

defaultConf = Conf

type Log = [(String,[String])]

type LogFile = (String, [String])

type FeatureVector = V.Vector Double

type Trace = String

type Sample = (Double,FeatureVector)

data GuessworkException = ArrangeException String
                        | ParserException String
    deriving (Show,Typeable)

instance Exception GuessworkException
