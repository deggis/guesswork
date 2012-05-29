{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Statistics.PCA
-- Copyright   :  (c) A. V. H. McPhail 2010
-- License     :  GPL-style
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  portable
--
-- Principal Components Analysis
--
-----------------------------------------------------------------------------

module Numeric.Statistics.PCA (
                               pca, pcaTransform, PCAData, pcaReduce
                          ) where


-----------------------------------------------------------------------------

import qualified Data.Array.IArray as I 

import Numeric.LinearAlgebra

import Numeric.GSL.Statistics

import Numeric.Statistics

-----------------------------------------------------------------------------

data PCAData = PCAData { means :: [Double]
                       , eigenValues :: [Double]
                       , m :: Matrix Double } deriving Show

-- | find the n principal components of multidimensional data
pca :: I.Array Int (Vector Double)    -- the data
    -> Double                         -- eigenvalue threshold
    -> PCAData                        -- means + matrix
pca d q = let d' = fmap (\x -> x - (scalar $ mean x)) d -- remove the mean from each dimension
              means = map mean . I.elems $ d
              cv = covarianceMatrix d'
              (val',vec') = eigSH cv           -- the covariance matrix is real symmetric
              eigenValues = toList val'
              vec = toColumns vec'
              v' = zip eigenValues vec
              v = filter (\(x,_) -> x > q) v'  -- keep only eigens > than parameter
              m = fromColumns $ snd $ unzip v
          in PCAData{..}

-- | perform a PCA transform of the original data (remove mean)
-- |     Final = M^T Data^T
pcaTransform :: I.Array Int (Vector Double)    -- ^ the data
             -> PCAData                        -- ^ the means + principal components
             -> I.Array Int (Vector Double)    -- ^ the transformed data
pcaTransform d PCAData{..} =
    let vecs = map (\(ix,v) -> v - (scalar $ means !! ix)) . zip [0..] . I.elems $ d
    in I.listArray (1,cols m) $ toRows $ (trans m) <> (fromRows vecs)

-- | perform a dimension-reducing PCA modification
pcaReduce :: I.Array Int (Vector Double)      -- ^ the data
          -> Double                           -- ^ eigenvalue threshold
          -> I.Array Int (Vector Double)      -- ^ the reduced data, with n principal components
pcaReduce d q = let u = fmap (scalar . mean) d
                    d' = zipWith (-) (I.elems d) (I.elems u)
                    cv = covarianceMatrix $ I.listArray (I.bounds d) d'
                    (val',vec') = eigSH cv           -- the covariance matrix is real symmetric
                    val = toList val'
                    vec = toColumns vec'
                    v' = zip val vec
                    v = filter (\(x,_) -> x > q) v'  -- keep only eigens > than parameter
                    m = fromColumns $ snd $ unzip v
                 in I.listArray (I.bounds d) $ zipWith (+) (toRows $ m <> (trans m) <> fromRows d') (I.elems u) 

-----------------------------------------------------------------------------
