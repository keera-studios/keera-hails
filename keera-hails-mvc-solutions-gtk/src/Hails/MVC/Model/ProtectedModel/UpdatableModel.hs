{-# LANGUAGE FlexibleInstances #-}
-- |
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Hails.MVC.Model.ProtectedModel.UpdatableModel where

import Hails.MVC.Model.ReactiveModel
import Hails.MVC.Model.ProtectedModel

import Data.ExtraVersion

import Hails.MVC.Model.ProtectedModel.VersionedModel

class VersionedBasicModel a => UpdatableBasicModel a where
  getBMUpdateURI :: a -> String
  getBMMaxVersionAvail :: a -> Maybe Version
  setBMMaxVersionAvail :: a -> Version -> a

class VersionedProtectedModel a => UpdatableProtectedModel a where
  getUpdateURI       :: a -> IO String
  getMaxVersionAvail :: a -> IO (Maybe Version)
  setMaxVersionAvail :: a -> Version -> IO ()

class Event a => UpdateNotifiableEvent a where
  updateNotificationEvent :: a

instance (UpdatableBasicModel a, UpdateNotifiableEvent b) => UpdatableProtectedModel (ProtectedModel a b) where
  getUpdateURI = (`onReactiveModel` getRMUpdateURI)
    where getRMUpdateURI = getBMUpdateURI . basicModel
  getMaxVersionAvail = (`onReactiveModel` getRMMaxVersionAvail)
    where getRMMaxVersionAvail = getBMMaxVersionAvail . basicModel
  setMaxVersionAvail pm v = pm `applyToReactiveModel` setRMMaxVersionAvail
    where setRMMaxVersionAvail rm = let rm' =  rm `onBasicModel` (`setBMMaxVersionAvail` v)
                                    in triggerEvent rm' updateNotificationEvent
