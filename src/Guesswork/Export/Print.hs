module Guesswork.Export.Print where

import Control.Monad.RWS.Strict
import qualified Guesswork.Analyze as ANALYZE
import Guesswork.Types

spit :: (Sample a) => ANALYZE.Analyzed a -> Guesswork ()
spit = liftIO . print
