-- |
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module CombinedEnvironment
   ( CRef
   , CEnv
   , GEnv.createCEnv
   , view
   , GEnv.model
   , createCRef
   , readIORef
   , writeIORef
   , updateView
   , onViewAsync
   )
  where

import Data.IORef

-- Internal libraries
import qualified Hails.MVC.GenericCombinedEnvironment as GEnv
import qualified Hails.MVC.View.GtkView               as GView

import View
import Model.ReactiveModel.ModelEvents
import Model.Model

type CEnv = GEnv.CEnv View Model ModelEvent
type CRef = IORef CEnv

view :: CEnv -> View
view = GView.getGUI . GEnv.view

updateView :: CEnv -> View -> CEnv
updateView cenv v = cenv { GEnv.view = GView.GtkView v }

createCRef :: Model -> IO CRef
createCRef mb = newIORef =<< GEnv.createCEnv mb
