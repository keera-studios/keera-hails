-- |
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Model.ProtectedModel.ProtectedModelInternals
   ( ProtectedModel
   , GPM.onReactiveModel
   , GPM.applyToReactiveModel
   , GPM.onEvent
   , GPM.waitFor
   )
  where

import qualified Hails.MVC.Model.ProtectedModel as GPM

import Model.Model
import Model.ReactiveModel.ModelEvents

type ProtectedModel = GPM.ProtectedModel Model ModelEvent
