{-# LANGUAGE TemplateHaskell #-}
-- | This module holds the functions to access and modify the project name
-- in a reactive model.
module Model.ProtectedModel.ProtectedFields where

-- Internal imports
import Hails.MVC.Model.THFields
import Hails.MVC.Model.ProtectedModel.Reactive

import Model.Model
import qualified Model.ReactiveModel as RM
import Model.ReactiveModel.ModelEvents
import Model.ProtectedModel.ProtectedModelInternals

-- protectedField {- Model field -} {- Field type -}    {- Model name -} {- event name -}
-- protectedField "Language"        [t|Maybe Language|] "Model"          "ModelEvent"
