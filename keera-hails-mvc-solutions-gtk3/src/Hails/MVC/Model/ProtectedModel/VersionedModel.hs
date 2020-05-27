-- |
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Hails.MVC.Model.ProtectedModel.VersionedModel where

import Data.ExtraVersion

import Hails.MVC.Model.ProtectedModel
import Hails.MVC.Model.ReactiveModel
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.DeriveField

class VersionedBasicModel a where
  getBMVersion :: a -> Version

class VersionedProtectedModel a where
  getVersion :: a -> IO Version

-- class VersionedReactiveModel a where
--   getRMVersion :: a -> String
-- 
-- instance VersionedBasicModel a => VersionedReactiveModel (ReactiveModel a) where
--   getRMVersion = getBMVersion . basicModel

instance (Event b, VersionedBasicModel a) => VersionedProtectedModel (ProtectedModel a b) where
  getVersion = (`onReactiveModel` getRMVersion)
    where getRMVersion = getBMVersion . basicModel

deriveVersioned :: Name -> Q [Dec]
deriveVersioned =
  deriveField "VersionedBasicModel" "getBMVersion" "version"
