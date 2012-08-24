{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
module Guesswork.Types where

import Control.Monad.RWS.Strict
import Control.Exception
import Control.Applicative
import Data.Typeable
import Data.Serialize as S
import GHC.Generics
import qualified Data.Vector.Unboxed as V

type Guesswork a = RWST Conf Log () IO a

data Conf = Conf

defaultConf = Conf

type Log = [(String,[String])]

type LogFile = (String, [String])

type FeatureVector = V.Vector Double

type Trace = String

class Transformable a where
    transform :: (FeatureVector -> FeatureVector) -> a -> a

class (Show a, Eq a, Transformable a) => Sample a where
    target   :: a -> Double
    features :: a -> FeatureVector

toPair :: (Sample a) => a -> (Double,FeatureVector)
toPair x = (target x, features x)

newtype SamplePair = SP (Double,FeatureVector)
    deriving (Show,Generic,Eq)

instance Serialize SamplePair

instance Sample SamplePair where
    target   (SP (x,_)) = x
    features (SP (_,f)) = f

instance Transformable SamplePair where
    transform f (SP (x,v)) = SP (x, f v)

instance Serialize (V.Vector Double) where
    get = V.fromList <$> S.get
    put = S.put . V.toList

data GuessworkException = ArrangeException String
                        | ParserException String
    deriving (Show,Typeable)

instance Exception GuessworkException

class (Serialize a) => GuessworkEstimator a where
    guessWith :: a -> FeatureVector -> Double
