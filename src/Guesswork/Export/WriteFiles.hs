module WriteFiles where

import Control.Monad.RWS.Strict
import AI.UASI.Utils.StringTools
import Data.List
import Types
import Logging

l = "tundra.writefiles"

saveExtractedFs :: [TExtracted] -> Tundra [TExtracted]
saveExtractedFs xs = do
    info l "Saving extracted features to files"
    mapM_ saveExtracted xs
    info l "Done."
    return xs

saveExtracted :: TExtracted -> Tundra ()
saveExtracted TExtracted{..} = do
    Conf{..} <- ask
    let formatter (KoealaData{..},fs) = plotToString (fromIntegral $ koealaid ala, fs)
        content = unlines . map formatter $ samples
        fn      = targetPath++"extracted_"++extractionBranch++".txt"
    info l $ "Writing featureset to " ++ fn
    liftIO $ writeFile fn content

saveDataSets :: [TDataSet] -> Tundra [TDataSet]
saveDataSets xs = do
    info l "Saving processed datasets to files"
    mapM_ saveDataSet xs
    info l "Done."
    return xs

saveDataSet :: TDataSet -> Tundra ()
saveDataSet TDataSet{..} = do
    Conf{..} <- ask
    let formatter = plotToString
        content = unlines . map formatter
        fn t  = targetPath++"dataset_"++dataSetBranch++"_"++t++".txt"
    info l $ "Writing featuresets to " ++ fn "[train/test]" ++ ".txt"
    liftIO $ writeFile (fn "train") (content train)
    liftIO $ writeFile (fn "test") (content test)
    debug l $ concat ["Dataset; "
                     ,dataSetBranch
                     ,", train/test: "
                     ,show (length train)
                     ,"/"
                     ,show (length test)]
