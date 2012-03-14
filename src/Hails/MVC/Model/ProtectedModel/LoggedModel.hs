{-# LANGUAGE FlexibleInstances #-}
module Hails.MVC.Model.ProtectedModel.LoggedModel where

import Hails.MVC.Model.ProtectedModel
import Hails.MVC.Model.ReactiveModel
import Control.Monad
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.DeriveField
import System.Log.Logger

class LoggedBasicModel a where
  getBMLogName :: a -> String

class LoggedProtectedModel a where
  getLogName :: a -> IO String
  getLog     :: a -> IO Logger

instance (Event b, LoggedBasicModel a) => LoggedProtectedModel (ProtectedModel a b) where

  getLogName = (`onReactiveModel` getRMLogName)
    where getRMLogName = getBMLogName . basicModel

  getLog = getLogName >=> getLogger

deriveLogged :: Name -> Q [Dec]
deriveLogged =
  deriveField "LoggedBasicModel" "getBMLogName" "logName"
