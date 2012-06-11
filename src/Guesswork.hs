{-# LANGUAGE RecordWildCards #-}

{-|
    This module contains minimal imports for Guesswork and defines RWST Guesswork
    type.

-}
module Guesswork where

import Guesswork.Types

-- paketti, josta pitää vielä valita kohdeattribuutti
--type FatSample = (KoealaData, Features)

-- estimoitava arvo (puumäärä) + piirteet
--type Sample = (Double,Features)

--class Sample a where
--    attribute :: String -> a -> NamedValue
--    features  :: a -> FeatureVector

data NamedValue = NamedValue { value :: Double, name :: String }


-- type Estimator = [Sample] -> [Sample] -> EstimationResults
-- 
-- data EstimationResults = EstimationResults { truth :: [Double]
--                                            , predicted :: [Double] }
--     deriving (Show)
-- 
-- type KoealaFeature = KoealaData -> Double -- tai [Double] ?
-- 
-- data TInfos = TInfos { infos :: [Koeala] }
-- 
-- data TImages = TImages { images :: [KoealaData] }
-- 

