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

data ScaleConfig = ScaleConfig { toRange :: (Double,Double) }
    deriving (Show,Eq)

data Method = Scale ScaleConfig
            | Pass
    deriving (Show,Eq)

data Operation = ScaleOp { config :: ScaleConfig
                         , ranges :: [(Double,Double)] }
               | PassOp
    deriving (Show)

data Transformed = Separated { train :: [Sample]
                 , test :: [Sample]
                 , trace :: Trace }
                 | LeaveOneOut [Sample] Trace
                 | OnlyTrain [Sample] Operation Trace
    deriving(Show)

defaultScaleConfig = ScaleConfig (0,1)

-- |Scaling transformation: scales data values between [0,1]
-- based on feature ranges of train data points.
scale :: ARRANGE.Arranged -> Guesswork Transformed
scale = scale' defaultScaleConfig

-- |Scaling transformation: scales data values between given
-- range based on feature ranges of train data points.
scale' :: ScaleConfig -> ARRANGE.Arranged -> Guesswork Transformed
scale' config (ARRANGE.Separated train test trace) = do
    let op     = getTransform (Scale config) . map snd $ train
        train' = applyToFs op train
        test'  = applyToFs op test
    return $ Separated train' test' (trace ++ ",D=scaling")
scale' config (ARRANGE.LeaveOneOut samples trace) = do
    let op     = getTransform (Scale config) . map snd $ samples
    return $ LeaveOneOut (applyToFs op samples) (trace ++ ",D=scaling")
scale' config (ARRANGE.OnlyTrain train trace) = do
    let op     = getTransform (Scale config) . map snd $ train
        train' = applyToFs op train
    return $ OnlyTrain train' op (trace ++ ",D=scaling")

-- |No-op, just pass the data without doing any transformations.
pass :: ARRANGE.Arranged -> Guesswork Transformed
pass (ARRANGE.Separated train test trace) = do
    let trace' = trace ++ ",D=pass"
    return $ Separated train test trace
pass (ARRANGE.LeaveOneOut samples trace) = do
    let trace' = trace ++ ",D=pass"
    return $ LeaveOneOut samples trace'
pass (ARRANGE.OnlyTrain train trace) = do
    return $ OnlyTrain train PassOp (trace ++ ",D=pass")

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
apply (ScaleOp ScaleConfig{..} ranges) vec = S.scaleUsingRanges toRange ranges vec
apply PassOp                   vec = vec
--apply PCAOp{..} vec =
--     let box a  = [a]
--         pack   = pcaPackVecs . S.rotate2DArray . box
--         unpack = head . pcaUnpackVecs
--         scaled = apply scaleOp' vec
--     in  unpack $ PCA.pcaTransform (pack scaled) pcaData

getTransform :: Method -> [FeatureVector] -> Operation
getTransform (Scale config) trainData   = ScaleOp config $ S.calcRanges trainData
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


