-- |
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Model.Model where

import Data.ExtraVersion
import Hails.MVC.Model.ProtectedModel.NamedModel
import Hails.MVC.Model.ProtectedModel.VersionedModel

type BasicModel = Model

data Model = Model
 { name              :: String
 , version           :: Version
 }
 deriving (Eq)

emptyBM :: Model
emptyBM = Model
 { name              = "Elementary Gtk Demo with PRMVC"
 , version           = Version 0 1 Alpha 0
 }

instance VersionedBasicModel Model where
 getBMVersion = version

instance NamedBasicModel Model where
 getBMName = name
