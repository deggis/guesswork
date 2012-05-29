module Reckon.Separation where

data Separated = Separated { train :: [a] -- Sample a
                           , test  :: [a]
                           , trace :: Trace }
    deriving Show

