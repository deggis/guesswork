-- Ajatus: aineiston pilkkomistavat opetus/testausaineistoksi tähän.
{-# LANGUAGE RecordWildCards #-}

module Separation (
    separateByTreeVolumes 
   ,splitSetsToSpecies ) where

import Control.Arrow
import Data.List
import Data.Ord
import qualified Data.Vector.Unboxed as VU
import AI.UASI.TransformOperation
import Math.KMeans
import Data.Maybe
import Types
import Logging

-- | Separates samples with kmeans using given function to extract
-- features. Extracted features are scaled to range [0,1] between k-means.
-- Provide the function to return identifier of FatSamples.
--
separateDataKMeans :: (Eq a) => (FatSample -> [Double]) ->
                                (FatSample -> a) ->
                                Int ->
                                [FatSample] ->
                                Tundra ([a],[a])
separateDataKMeans extract attr nClusters samples = do
    debug l $ "Separating identifiers with kmeans using " ++ show nClusters ++ " clusters."
    let packs       = map (\s->(extract s, s)) samples
        scalingOp   = getTransform Scale . map fst $ packs
        scaledPacks = map (first $ apply scalingOp) packs
        -- Create clusters
        clusters    = kmeans nClusters . map (first VU.fromList) $ scaledPacks
        -- Flatten clusters and pick data using 1:2.
        ordered     = zip (cycle [1..3]) . map snd . concat $ clusters
        picker p    = map (attr . snd) . filter (\(i,_) -> p i) $ ordered
--        lookup' i   = fromJust $ lookup i packs
        trainD      = picker (<3)
        testD       = picker (==3)
    debug l $ "Separated identifiers using k-means. Cluster sizes: " ++ (show . map length $ clusters)
    return (trainD,testD)
  where
    l = "tundra.separation.kmeans"
