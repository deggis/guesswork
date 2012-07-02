module Guesswork.Import.CSVUtils where

import qualified Data.Attoparsec.Text as P
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- |Simple evaluator for selecting lines to read.
isDataLine :: T.Text -> Bool
isDataLine t | T.head t == '#' = False
             | T.length t < 8  = False
             | otherwise       = True

-- |Crude method of reading a file. Errors are thrown.
forceRead :: P.Parser a -> T.Text -> a
forceRead p t =
    case P.parseOnly p t of
        Left s  -> error $ "Got "++s++" while reading "++T.unpack t
        Right x -> x

-- |Reads a file with given parser.
parseFile :: P.Parser a -> FilePath -> IO [a]
parseFile p fp = do
    lns <- fmap T.lines $ T.readFile fp
    return . map (forceRead p) . filter isDataLine $ lns

