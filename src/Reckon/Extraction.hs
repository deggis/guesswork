module Reckon.Extraction where

data Extracted = Extracted { samples :: [a] -- Sample a
                           , trace :: Trace }
    deriving Show

