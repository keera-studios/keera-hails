module CombinedEnvironment
   ( CRef
   , view
   , GEnv.model
   , GEnv.createCRef
   , GEnv.readIORef
   , GEnv.writeIORef
   , updateView
   , onViewAsync
   )
  where

-- Internal libraries
import qualified Graphics.UI.Gtk.GenericView as GView
import qualified GenericCombinedEnvironment as GEnv

import View
import Model.ReactiveModel.ModelEvents
import Model.Model

type CEnv = GEnv.CEnv View Model ModelEvent
type CRef = GEnv.CRef View Model ModelEvent

view :: CEnv -> View
view = GView.getGUI . GEnv.view

updateView :: CEnv -> View -> CEnv
updateView cenv v = cenv { GEnv.view = GView.GtkView v }
