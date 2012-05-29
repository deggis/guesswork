module Logging where

import Control.Monad.RWS.Strict
import System.Log.Logger

logM' l p s = liftIO $ do
    logM l p s

info l s = logM' l INFO s
debug l s = logM' l DEBUG s
critical l s = logM' l CRITICAL s
