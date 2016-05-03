module Model.ProtectedModel.ProtectedModelInternals
   ( ProtectedModel
   , GPM.onReactiveModel
   , GPM.applyToReactiveModel
   , GPM.onEvent
   , GPM.waitFor
   )
  where

import Model.Model
import Model.ReactiveModel.ModelEvents
import qualified Control.Concurrent.Model.ProtectedModel as GPM

type ProtectedModel = GPM.ProtectedModel Model ModelEvent
