module Reckon.Export.Print where

import Control.Monad.RWS.Strict
import qualified Reckon.Analyze as ANALYZE
import Reckon.Types

spit :: ANALYZE.Analyzed -> Reckon ()
spit = liftIO . print
