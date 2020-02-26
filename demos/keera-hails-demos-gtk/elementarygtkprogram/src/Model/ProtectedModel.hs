-- |
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Model.ProtectedModel
   ( ProtectedModel
   , onEvent
   , waitFor
   , module Exported
   )
  where

import Model.ProtectedModel.ProtectedModelInternals
import Model.ReactiveModel.ModelEvents as Exported
