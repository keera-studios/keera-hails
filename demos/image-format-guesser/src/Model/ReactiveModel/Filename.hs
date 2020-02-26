-- |
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Model.ReactiveModel.Filename where

import qualified Model.Model as M
import Model.ReactiveModel.ReactiveModelInternals
import Model.ReactiveModel.ModelEvents

setFilename :: ReactiveModel -> String -> ReactiveModel
setFilename rm s'
  | s == s'   = rm
  | otherwise = rm' `triggerEvent` FilenameChanged
 where s   = M.fileName $ basicModel rm
       rm' = onBasicModel rm (\b -> b { M.fileName = s' })

getFilename :: ReactiveModel -> String
getFilename = M.fileName . basicModel
