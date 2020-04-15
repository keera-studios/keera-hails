-- |
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Model.Model where

import Model.ProtectedModel.NamedModel
import Model.ProtectedModel.VersionedModel

type BasicModel = Model

data Model = Model
 { name     :: String
 , version  :: String
 , fileName :: String
 }
 deriving (Eq)

emptyBM :: Model
emptyBM = Model
 { name     = "Elementary Gtk Demo with PRMVC"
 , version  = "0.1-alpha0"
 , fileName = ""
 }

instance VersionedBasicModel Model where
 getBMVersion = version

instance NamedBasicModel Model where
 getBMName = name
