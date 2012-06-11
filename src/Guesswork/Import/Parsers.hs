-- Ajatus: tiedostojen lukemiseen liittyvät CSV/ENVI/TFW -parserit yms.
-- tänne, jotta irrallaan IO-kaluista.

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Parsers where

import qualified Data.Attoparsec.Text as P
import qualified Data.Attoparsec.Combinator as P
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Types
import CV.Image

comma = P.char ','
csInt s = P.decimal <* comma P.<?> s
csDouble s = P.double <* comma P.<?> s

puu :: P.Parser Puu
puu = do
    puu_koealaid <- csInt "koeala"
    nro <- csInt "nro"
    puulaji <- csInt "puulaji"

    latvkerr <- csInt "latvkerr"

    lpm <- csInt "lpm"
    pituus <- csInt "pituus"
    puu_x_euref <- csDouble "puu_x_euref"
    puu_y_euref <- csDouble "puu_y_euref" --  P.double P.<?> "puu_y_euref"
    P.skipSpace
    return $! Puu{..}

koeala :: P.Parser Koeala
koeala = do
    koealaid <- csInt "koealaid"
    x_euref <- csDouble "x_euref"
    y_euref <- csDouble "y_euref"
    vma <- csDouble "vma"
    vku <- csDouble "vku"
    vlp <- csDouble "vlp"
    vtot <- csDouble "vtot"
    gma <- csDouble "gma"
    gku <- csDouble "gku"
    gle <- csDouble "gle"
    g <- csDouble "g"
    nma <- csDouble "nma"
    nku <- csDouble "nku"
    nle <- csDouble "nle"
    n <- csDouble "n"
    dgm <- csDouble "dgm"
    hgm <- csDouble "hgm"
    xykj <- csDouble "x_ykj"
    yykj <- P.double P.<?> "y_ykj"
    P.skipSpace
    return $! Koeala{..}


parseEither s = P.parseOnly

-- Sääli kun attoparsecin Either ei palauta tarkempaa virhettä

parseEither' :: String -> P.Parser a -> T.Text -> Either String a
parseEither' s p t = case (P.parse (p P.<?> s) t) of
    P.Fail ct c e -> Left $ unlines [show t, show e, show c, show ct]
    P.Done ct r   -> Right r
    P.Partial x   -> Left $ unlines ["Partial "++s++" match with:", show t]

-- Parses GeoTiff TFW header file
-- http://gis.ess.washington.edu/data/raster/drg/tfw.html
parseTFWHeader :: (Int,Int) -> a -> String -> ImageInWorld a
parseTFWHeader (w,h) im s =
    let [a,b,c,d,e,f] = map read . lines . filter (/='\r') $ s
    in ImageInWorld{..}

-- Parses some header info from ENVI header file
-- http://geol.hu/data/online_help/ENVI_Header_Format.html
parseENVIHeader :: a -> String -> ImageInWorld a
parseENVIHeader im (T.pack->s) =
    let (P.Done _ res)            = P.parse (mapLinePref *> P.count 6 num) $ s
        [_,_,lrx,lry,xUnit,yUnit] = res
        h                         = 200 -- XXX: info available in envi header in lines samples/lines
        w                         = 200 --
        [a,b,c,d,e,f]             = [xUnit,0,0,-yUnit,lrx,lry]
    in ImageInWorld{..}
  where
    -- find and parse nums from line in format of
    -- "map info = {Unknown, 1, 1, 400898.697548, 6786343.147276, 0.2, 0.2, 0, North}"
    num         = P.double <* P.char ',' <* P.skipSpace
    mapLinePref = P.manyTill P.anyChar (P.try (pS "map info")) >>
                  P.skipSpace >>
                  P.char '=' >>
                  P.skipSpace >>
                  P.char '{' >>
                  P.manyTill P.anyChar (P.try (P.char ',')) >>
                  P.skipSpace


pS = P.string . T.pack


