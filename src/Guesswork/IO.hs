module Guesswork.IO where

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
sampleToText :: (Sample a) => a -> T.Text
sampleToText s =
    T.pack . concat $ [show x, " ", unwords vals]
  where
    vec  = features s
    x    = target s
    vals = map (\(i,v) -> concat [show i, ":", show v]) . zip [1..] . V.toList $ vec

-- |Parses a sample in format of "%f 1:%f 2:%f 3:%f 4:%f" where
-- %f represents a double.
sample :: P.Parser SamplePair
sample = do
    attribute <- pDouble "attribute"
    P.skipSpace
    let feature = P.decimal *> P.char ':' *> pDouble "featureValue"
    features <- P.many1 (feature <* P.skipSpace)
    return $ SP (attribute, V.fromList features)
  where
    pDouble s = P.double P.<?> s

-- |Reads samples from given filepath. See 'sample' for format.
readFeatureFile :: FilePath -> IO [SamplePair]
readFeatureFile fn = do
    lns <- fmap T.lines $ T.readFile fn
    return $ map parser lns
  where
    parser l =
        case P.parseOnly sample l of
            Left _ -> throw $ ParserException ("Parsing failed with line: "++T.unpack l)
            Right x -> x

-- |Writes samples to given filepath. See sample for format.
writeFeatureFile :: (Sample a) => FilePath -> [a] -> IO ()
writeFeatureFile path = T.writeFile path . T.unlines . map sampleToText
