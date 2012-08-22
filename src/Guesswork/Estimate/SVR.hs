{-# LANGUAGE RecordWildCards #-}
module Guesswork.Estimate.SVR
    (
      SVR
    , guessWith
    , svr
    , defaultSVR
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
import Data.Ord
import AI.SVM.Simple as SVM
import AI.SVM.Base as SVM


data SVR = SVR

instance GuessworkEstimator SVR where
    guessWith SVR = error "Not implemented"

-- | Using Epsilon SVR with given parameters
data SVRConfig = SVRConfig { epsilon :: Double
                           , kernel :: Kernel }
               | GridSearch { epsilon_candidates :: [Double]
                            , cost_candidates :: [Double]
                            , kernel_candidates :: [Kernel] }
    deriving (Show)

defaultSVR = SVRConfig 0.1 Linear

svr :: SVRConfig -> TRANSFORM.Transformed -> Guesswork Estimated
svr conf (TRANSFORM.Separated train test trace') = do
    let
        (trainSet,fitnessSet)  = splitAt (length train `div` 2) train
        (_, regressor, setups) = optimizeSVR trainSet fitnessSet conf
        testFeatures           = map snd test
        estimates              = map (SVM.predictRegression regressor) testFeatures
        truths                 = map fst test
        ((e,c,k),_)            = head setups
        trace                  = trace' ++ ",R=SVRRBF{ε=" ++ show e ++ ",C=" ++ show c ++ ",g=" ++ show k
    return Estimated{..}

optimizeSVR _      _        SVRConfig{..} = error "Not supported"
optimizeSVR trainD fitnessD GridSearch{..} =
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

