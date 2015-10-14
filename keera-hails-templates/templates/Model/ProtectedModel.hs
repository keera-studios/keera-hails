module Model.ProtectedModel
   ( ProtectedModel
   , onEvent
   , waitFor
   , module Exported
   )
  where

import Model.ProtectedModel.ProtectedModelInternals
import Model.ReactiveModel.ModelEvents               as Exported
import Model.ProtectedModel.ProtectedFields          as Exported
import Hails.MVC.Model.ProtectedModel.Initialisation as Exported