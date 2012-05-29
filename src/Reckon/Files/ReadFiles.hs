-- Ajatus: tiedostojenlukijahärpelit tänne

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module ReadFiles (
    readPlots
   ,loadPlots ) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Attoparsec.Text as P
import qualified Data.Map as M
import qualified Data.ByteString as B
import System.Log.Logger
import Control.Monad.RWS.Strict
import Control.DeepSeq
import Data.Array.CArray
import Data.Ord
import Data.List
import Data.Maybe
import System.FilePath.Posix
import CV.Conversions
import CV.Transforms
import CV.Image
import Parsers
import Types
import Logging
import FeaturesDEM (calcDEM)

readPlots :: Tundra TInfos
readPlots = do
    info l "Beginning to read CSV's."
    Conf{..} <- ask
    plots' <- lueKoealaCSVt
    case plots' of
        Right plots -> do
            info l $ "Done. Read " ++ show (length plots) ++ " plots."
            return $! TInfos . sortBy (comparing koealaid) $ plots
        Left s -> do
            logM' l CRITICAL s
            fail s
  where
    l = "tundra.csvreader"

loadPlots :: TInfos -> Tundra TImages
loadPlots TInfos{..} = do
    info l "Loading plots."
    loaded <- mapM load infos
    info l "Done."
    return $! TImages loaded
  where
    l = "tundra.imagereader"


--FIXME: korjaa nimeäminen. sekaisin kuvien ja CSV-tiedostojen lukemista.

-- Anna Koeala, riittää olla { koealaid = # }.
-- Ladataan CSV-tietojen pohjalta KoealaData jossa mukana kuvat.
load :: Koeala -> Tundra KoealaData
load ala@Koeala{..} = do
    Conf{..} <- ask
    cir <- liftIO $ loadCIR (cirFn koealaid plotBasePath)
    maa <- liftIO $ loadENVI (maaFn koealaid plotBasePath)
    latvus <- liftIO $ loadENVI (latvusFn koealaid plotBasePath)
    let dem = calcDEM latvus maa
    debug l $ "Loaded images from plot " ++ show koealaid ++ "."
    return $ KoealaData{..}
  where
    l = "tundra.imagereader"

lueFilu :: String -> P.Parser a -> [T.Text] -> Either String [a]
lueFilu s p ts = mapM (parseEither s p) ts

lueKoealaCSVt :: Tundra (Either String [Koeala])
lueKoealaCSVt = do
    Conf{..} <- ask
    r <- liftIO $ lueKoealaCSVt' plotFile usedIds
    return r

lueKoealaCSVt' :: FilePath -> [Int] -> IO (Either String [Koeala])
lueKoealaCSVt' koeala_fn kaytetytAlat = do
    koealaLns <- fmap (tail . T.lines) $ T.readFile koeala_fn
    case (lueFilu "koeala" koeala koealaLns) of
        Left s -> return $ Left s
        Right koealat' -> do
            let res = filter (\Koeala{..} -> elem koealaid kaytetytAlat) koealat'
            return $ Right res

maaFn,latvusFn,cirFn :: Int -> FilePath -> FilePath
maaFn    = fn' "maanpinta.envi"
latvusFn = fn' "latvus.envi"
cirFn    = fn' "cir.tif"

fn' s koealaId basePath = concat [basePath,"/",show koealaId, "/", s]

loadCIR :: FilePath -> IO (ImageInWorld (Image RGB D32))
loadCIR (dropExtension->path) = do
    let fn = path++".tif"
    i <- readFromFile fn
--    i <- case i' of
--            Just i  -> return i -- FIXME: load color image! BGR/RGB/3*GrayScale?
--            Nothing -> error $ "Image not found at "++fn
    hdr <- readFile (path++".tfw")
    return $ parseTFWHeader (getSize i) i hdr

loadENVI :: FilePath -> IO (ImageInWorld (Image GrayScale D32))
loadENVI (dropExtension->path) = do
    hdrS <- readFile (path++".hdr")
    let hdr = parseENVIHeader (undefined :: Image GrayScale D32)  hdrS
    loadENVIImage hdr (path++".envi")

loadENVIImage :: ImageInWorld (Image GrayScale D32) -> FilePath -> IO (ImageInWorld (Image GrayScale D32))
loadENVIImage i@ImageInWorld{..} fp = do
    bs <- B.readFile fp
    let ar  = fromJust $ unsafeByteStringToCArray ((0,0),(w-1,h-1)) bs
        im' = copyFCArrayToImage $ ar
        -- it seems image is read in wrong order and need to be rotated!
        -- FIXME: therefore reading non-square ENVI's likely to break
        fix = CV.Transforms.flip Vertical . rotate (pi/2)
    return $ i { im = fix im' }
