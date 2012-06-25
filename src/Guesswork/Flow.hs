{-# LANGUAGE RecordWildCards #-}
module Guesswork.Flow where

import Control.Monad.RWS.Strict
import Guesswork.Types

runGuesswork :: Guesswork a -> IO a
runGuesswork flow = runRWST flow defaultConf () >>= \(a,_,_) -> return a
