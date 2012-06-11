module Guesswork.Import.IO where

import Data.List.Split

import qualified Data.Attoparsec.Text as P
import qualified Data.Attoparsec.Combinator as P
import Control.Applicative
import Control.Exception
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector.Unboxed as V
import Guesswork.Types

-- |Converts a sample in format of "%f 1:%f 2:%f 3:%f 4:%f" where
-- %f represents a double.
sampleToText :: Sample -> T.Text
sampleToText (att, vec) =
    T.pack $ show att ++ " " ++ unwords vals
  where
    vals = map (\(i,v) -> show i ++ ":" ++ show v) . zip [1..] . V.toList $ vec

-- |Parses a sample in format of "%f 1:%f 2:%f 3:%f 4:%f" where
-- %f represents a double.
sample :: P.Parser Sample
sample = do
    attribute <- pDouble "attribute"
    P.skipSpace
    let feature = P.decimal *> P.char ':' *> pDouble "featureValue"
    features <- P.many1 (feature <* P.skipSpace)
    return $! (attribute, V.fromList features)
  where
    pDouble s = P.double P.<?> s

-- |Reads samples from given filepath. See 'sample' for format.
readFeatureFile :: FilePath -> IO [Sample]
readFeatureFile fn = do
    lns <- fmap T.lines $ T.readFile fn
    return $ map parser lns
  where
    parser l =
        case (P.parseOnly sample l) of
            Left _ -> throw $ ParserException ("Parsing failed with line: "++T.unpack l)
            Right x -> x

-- |Writes samples to given filepath. See sample for format.
writeFeatureFile :: FilePath -> [Sample] -> IO ()
writeFeatureFile path samples =
    T.writeFile path . T.unlines . map sampleToText $ samples
