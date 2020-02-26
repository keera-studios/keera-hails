-- | Contains only one operation to notify that the system's been
-- initialised.
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Hails.MVC.Model.ReactiveModel.Initialisation where

import Hails.MVC.Model.ReactiveModel
import Hails.MVC.Model.ReactiveModel.Events

initialiseSystem :: InitialisedEvent b
                 => ReactiveModel a b c -> ReactiveModel a b c
initialiseSystem = (`triggerEvent` initialisedEvent)
