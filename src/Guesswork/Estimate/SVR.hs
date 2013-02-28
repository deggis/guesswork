{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}
module Guesswork.Estimate.SVR
    (
      SVR
    , guessWith
    , svr
    , defaultSVR
    , trainSVR
    , SVRConfig (..)
    -- * svm-simple options
    , Kernel(..)
    ) where

import AI.SVM.Base

import Guesswork.Types
import Guesswork.Math.Statistics
import qualified Guesswork.Transform as TRANSFORM
import Guesswork.Estimate
import Data.List
import GHC.Generics
import Control.Applicative
import Data.Ord
import qualified Data.Binary as B
import Data.Serialize
import AI.SVM.Simple as SVM
import AI.SVM.Base as SVM
import Debug.Trace
    

data SVR = SVR SVMRegressor TRANSFORM.Operation Trace
    deriving (Generic,Eq)

instance Serialize SVR

instance Eq SVMRegressor where
    a == b = error "SVMRegressor Eq not implemented!"

instance Serialize SVMRegressor where
    get = B.decode <$> get
    put = put . B.encode

instance GuessworkEstimator SVR where
    guessWith (SVR svr op _ ) = SVM.predictRegression svr . TRANSFORM.apply op

-- | Using Epsilon SVR with given parameters
data SVRConfig =
     SVRConfig { epsilon :: Double
               , cost :: Double
               , kernel :: Kernel                         
               }                                          
   | GridSearch { epsilon_candidates :: [Double]          
                , cost_candidates :: [Double]             
                , kernel_candidates :: [Kernel]           
                }
  deriving (Show)

defaultSVR = SVRConfig 0.1 1.0 Linear

svr :: (Sample a) => SVRConfig -> TRANSFORM.Transformed a -> Guesswork (Estimated a)
svr conf (TRANSFORM.Separated train test t) = do
    let
        testFeatures           = map features test
        (_, regressor, setups) = foo train conf
        estimates              = map (SVM.predictRegression regressor) testFeatures
        truths                 = map target test
        trace                  = t -- ++ (showSVR . fst . head $ setups)
        samples                = test
    return Estimated{..}

foo train conf = 
    case conf of
        conf@GridSearch{..} ->
            let 
                (trainSet,fitnessSet)  = splitAt (length train `div` 2) train
            in optimizeSVR trainSet fitnessSet conf
        SVRConfig{..} ->
            let (m,s) = SVM.trainRegressor (SVM.Epsilon cost epsilon) kernel $ map toPair train
            in traceShow m $ (m, s, [])


showSVR :: (Double,Double,Kernel) -> String
showSVR (e,c,k) = "EpsilonSVR{eps=" +! e ++ ",C=" +! c ++ ",kernel=" +! k

trainSVR :: (Sample a) => SVRConfig -> TRANSFORM.Transformed a -> Guesswork SVR
trainSVR conf (TRANSFORM.OnlyTrain samples op t) = do
    let
        (_, svr, setups)      = foo samples conf
        trace                 = t -- ++ (showSVR . fst . head $ setups)
    return $ SVR svr op trace

optimizeSVR _      _        SVRConfig{..} = error "Not supported"
optimizeSVR (map toPair->trainD) (map toPair->fitnessD) GridSearch{..} =
    let
        svrSetups       = [ (e,c,k) | e<-epsilon_candidates
                                    , c<-cost_candidates
                                    , k<-kernel_candidates ]
        svrWithPoints   = map (\s -> (s, fitnessSVR trainD fitnessD s)) svrSetups
        sorted          = sortBy (comparing snd) svrWithPoints

        ((e,c,kernel),err)   = head sorted
        (msg, svr) = SVM.trainRegressor (SVM.Epsilon c e) kernel $ trainD ++ fitnessD
    in (msg, svr, sorted)
  where
    takeN = 10
--    epsilon_candidates   = [ 2**i | i<-[(-12),(-10)..6] ]
--    cost_candidates      = take 6 $ [ 1,8..512 ]
--    rbf_gamma_candidates = [1] -- [ 2**i | i<-[(-9)..1] ]

fitnessSVR trainD fitnessD (e,c,kernel) =
    let
        svr       = snd . SVM.trainRegressor (SVM.Epsilon c e) kernel $ trainD
        estimates = map (SVM.predictRegression svr . snd) fitnessD
        truths    = map fst fitnessD
    in calcFitness truths estimates

