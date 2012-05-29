{-# LANGUAGE RecordWildCards #-}
module Reckon.Transform where

import Data.Binary

import qualified Data.Array as A
import qualified Data.Packed.Vector as V
import qualified Data.Packed.Matrix as M
import qualified Numeric.Statistics.PCA as PCA

import AI.UASI.Utils.Misc
import AI.UASI.CommonTypes

import Reckon

type Transformer = FeatureVector -> FeatureVector

data Transformed = Transformed { train :: [a] -- Sample a
                               , test  :: [a]
                               , trace :: Trace }
    deriving Show

data Method = Scale -- No reduction, just scale data.
            | PCA { eigenThreshold :: Double }
            | Pass -- Pass data
    deriving (Show,Eq)

data Operation = ScaleOp { ranges :: [(Double,Double)] }
               | PCAOp { pcaData  :: PCA.PCAData
                       , scaleOp' :: Operation }
               | PassOp
    deriving (Show)

-- instance Binary Operation where
--     put (ScaleOp ranges) = do put (0 :: Word8)
--                               put ranges
--     get = do t <- get :: Get Word8
--              case t of
--                 0 -> do ranges <- get
--                         return (ScaleOp ranges)
--                 _ -> error "Unsupported transform operation."

apply :: Operation -> FeatureVector -> FeatureVector
apply (ScaleOp ranges) vec = scaleUsingRanges vec ranges
apply Pass             vec = vec
apply PCAOp{..} vec =
     let box a  = [a]
         pack   = pcaPackVecs . rotate2DArray . box
         unpack = head . pcaUnpackVecs
         scaled = apply scaleOp' vec
     in  unpack $ PCA.pcaTransform (pack scaled) pcaData

getTransform :: Method -> [FeatureVector] -> Operation
getTransform Scale trainData   = ScaleOp $ calcRanges trainData
getTransform PCA{..} trainData' =
    let pack      = pcaPackVecs . rotate2DArray
        scaleOp'  = getTransform Scale trainData'
        trainData = map (apply scaleOp') trainData'
        pcaData   = PCA.pca (pack trainData) eigenThreshold
    in PCAOp{..}
getTransform Pass = PassOp

pcaPackVecs :: [[Double]] -> A.Array Int (V.Vector Double)
pcaPackVecs rarr = A.array (1,length rarr) $ zip [1..] $ map V.fromList rarr

pcaUnpackVecs :: A.Array Int (V.Vector Double) -> [[Double]]
pcaUnpackVecs = rotate2DArray . map V.toList . A.elems
