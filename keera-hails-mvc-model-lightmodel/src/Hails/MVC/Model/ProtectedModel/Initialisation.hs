-- | Contains only one operation to notify that the system's been
-- initialised.
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Hails.MVC.Model.ProtectedModel.Initialisation where

import qualified Hails.MVC.Model.ReactiveModel.Initialisation as RM
import Hails.MVC.Model.ReactiveModel.Events
import Hails.MVC.Model.ProtectedModel

initialiseSystem :: InitialisedEvent c => ProtectedModel a c -> IO ()
initialiseSystem = (`applyToReactiveModel` RM.initialiseSystem)
