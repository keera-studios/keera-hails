-- | Contains the protected model definition used by other modules to
-- declare the protected fields.
--
-- FIXME: In a very rails-like move, this module will likely be
-- exactly the same for all programs, so we should try to put
-- a "Convention-over-configuration" policy in place and remove this
-- unless it must be adapted by the user.
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
import qualified Hails.MVC.Model.ProtectedModel as GPM

type ProtectedModel = GPM.ProtectedModel Model ModelEvent
