{-# LANGUAGE RecordWildCards #-}
module Guesswork.Transform where

import Control.Arrow

--import qualified Data.Array as A
--import qualified Data.Packed.Vector as V
--import qualified Data.Packed.Matrix as M
--
--import qualified Guesswork.Transform.PCA as PCA

import qualified Data.Vector.Unboxed as V
import qualified Guesswork.Transform.Scale as S

import Guesswork.Types
import qualified Guesswork.Arrange as ARRANGE

type Transformer = ARRANGE.Arranged -> Guesswork Transformed

data Method = Scale -- No reduction, just scale data.
--            | PCA { eigenThreshold :: Double }
            | Pass -- Pass data
    deriving (Show,Eq)

data Operation = ScaleOp { ranges :: [(Double,Double)] }
--               | PCAOp { pcaData  :: PCA.PCAData
--                       , scaleOp' :: Operation }
               | PassOp
    deriving (Show)

data Transformed = Separated { train :: [Sample]
                             , test :: [Sample]
                             , trace :: String }
                 | LeaveOneOut { samples :: [Sample]
                               , trace' :: String }
    deriving(Show)

-- |Scaling transformation: scales data values between [0,1]
-- based on feature ranges of train data points.
-- FIXME: enable different scaling methods.
scale :: ARRANGE.Arranged -> Guesswork Transformed
scale (ARRANGE.Separated train test trace) = do
    let op     = getTransform Scale . map snd $ train
        train' = applyToFs op train
        test'  = applyToFs op test
        trace' = trace ++ ",D=scaling"
    return $ Separated train' test' trace'
scale (ARRANGE.LeaveOneOut samples trace) = do
    let trace' = trace ++ ",D=scaling"
    return $ LeaveOneOut samples trace'

-- |No-op, just pass the data without doing any transformations.
pass :: ARRANGE.Arranged -> Guesswork Transformed
pass (ARRANGE.Separated train test trace) = do
    let trace' = trace ++ ",D=pass"
    return $ Separated train test trace
pass (ARRANGE.LeaveOneOut samples trace) = do
    let trace' = trace ++ ",D=pass"
    return $ LeaveOneOut samples trace'

applyToFs f = map (second (apply f))

--pcaSet :: TSeparated -> TProcessed
--pcaSet TSeparated{..} =
--    let op = getTransform (PCA 0.1) $ map snd trainFat
--        trainFat' = applyToFs op trainFat
--        testFat'  = applyToFs op testFat
--        processBranch = separationBranch ++ ",D=PCA"
--    in TProcessed{..}

-- instance Binary Operation where
--     put (ScaleOp ranges) = do put (0 :: Word8)
--                               put ranges
--     get = do t <- get :: Get Word8
--              case t of
--                 0 -> do ranges <- get
--                         return (ScaleOp ranges)
--                 _ -> error "Unsupported transform operation."

apply :: Operation -> FeatureVector -> FeatureVector
apply (ScaleOp ranges) vec = S.scaleUsingRanges vec ranges
apply PassOp           vec = vec
--apply PCAOp{..} vec =
--     let box a  = [a]
--         pack   = pcaPackVecs . S.rotate2DArray . box
--         unpack = head . pcaUnpackVecs
--         scaled = apply scaleOp' vec
--     in  unpack $ PCA.pcaTransform (pack scaled) pcaData

getTransform :: Method -> [FeatureVector] -> Operation
getTransform Scale trainData   = ScaleOp $ S.calcRanges trainData
--getTransform PCA{..} trainData' =
--    let pack      = pcaPackVecs . S.rotate2DArray
--        scaleOp'  = getTransform Scale trainData'
--        trainData = map (apply scaleOp') trainData'
--        pcaData   = PCA.pca (pack trainData) eigenThreshold
--    in PCAOp{..}
getTransform Pass _ = PassOp

--pcaPackVecs :: [[Double]] -> A.Array Int (V.Vector Double)
--pcaPackVecs rarr = A.array (1,length rarr) $ zip [1..] $ map V.fromList rarr
--
--pcaUnpackVecs :: A.Array Int (V.Vector Double) -> [[Double]]
--pcaUnpackVecs = S.rotate2DArray . map V.toList . A.elems
--


