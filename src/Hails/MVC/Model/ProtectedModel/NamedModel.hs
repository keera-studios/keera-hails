module Hails.MVC.Model.ProtectedModel.NamedModel where

import Hails.MVC.Model.ProtectedModel
import Hails.MVC.Model.ReactiveModel
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.DeriveField

class NamedBasicModel a where
  getBMName :: a -> String

class NamedProtectedModel a where
  getName :: a -> IO String

-- class NamedReactiveModel a where
--   getRMName :: a -> String
-- 
-- instance NamedBasicModel a => NamedReactiveModel (ReactiveModel a) where
--   getRMName = getBMName . basicModel

instance (Event b, NamedBasicModel a) => NamedProtectedModel (ProtectedModel a b) where
  getName = (`onReactiveModel` getRMName)
   where getRMName = getBMName . basicModel

deriveNamed :: Name -> Q [Dec]
deriveNamed =
  deriveField "NamedBasicModel" "getBMName" "name"
