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

-- | Separates data with k-means using all different tree volumes available.
-- Separation rules from first set is applied to rest.
separateByTreeVolumes :: [TExtracted] -> Tundra [TSeparated]
separateByTreeVolumes exts@(reference:_) = do
    let val f = f . ala . fst-- | Value from FatSample
        extractor x = map (\f -> f x) [val vtot
                                      ,val vma
                                      ,val vku
                                      ,val vlp]
        attribute    = val koealaid
        nClusters    = 15
        ref          = samples reference
    (trainIds, testIds) <- separateDataKMeans extractor attribute nClusters ref
    debug l $ "Separation complete."
    debug l $ "Train ids: " ++ show trainIds
    debug l $ "Test ids: " ++ show testIds
    let sets = map (separateUsingIdentifiers (trainIds,testIds)) exts
    info l "Done."
    return sets
  where
    l = "tundra.separation"

separateUsingIdentifiers ::([Int],[Int]) -> TExtracted -> TSeparated
separateUsingIdentifiers (trainIds,testIds) TExtracted{..} =
    let pickIds ids = filter (\fs->elem (koealaid . ala . fst $ fs) ids) samples
        trainFat = pickIds trainIds
        testFat  = pickIds testIds
        separationBranch = extractionBranch ++ ",S=kmeans"
    in TSeparated{..}

splitSetsToSpecies :: [TProcessed] -> Tundra [TDataSet]
splitSetsToSpecies ts = do
    info l "Splitting extracted sets to sets by species."
    return . concat . map splitSetToSpecies $ ts
  where
    l = "tundra.splitdata"

splitSetToSpecies :: TProcessed -> [TDataSet]
splitSetToSpecies t = [ getClassFromSet t (vtot . ala) "C=vtot"
                      , getClassFromSet t (vma . ala) "C=vma"
                      , getClassFromSet t (vku . ala) "C=vma"
                      , getClassFromSet t (vlp . ala) "C=vlp"
                      ]

getClassFromSet :: TProcessed -> (KoealaData -> Double) -> String -> TDataSet
getClassFromSet TProcessed{..} x name =
    let train = map (first x) trainFat'
        test  = map (first x) testFat'
        dataSetBranch = concat [processBranch, ",", name]
    in TDataSet{..}
