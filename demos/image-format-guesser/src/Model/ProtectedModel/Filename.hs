-- |
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Model.ProtectedModel.Filename where

import Model.ProtectedModel.ProtectedModelInternals
import qualified Model.ReactiveModel as RM

setFilename :: ProtectedModel -> String -> IO ()
setFilename pm fn = applyToReactiveModel pm (`RM.setFilename` fn)

getFilename :: ProtectedModel -> IO String
getFilename = (`onReactiveModel` RM.getFilename)
