-- | Implements the generic view class for the Gtk GUI manager.
module Graphics.UI.Gtk.GtkView where

import Graphics.UI.View
import Graphics.UI.Gtk

-- | A GtkGUI is a collection of elements that may be initialised We use this
-- class to make the Instantiation of View simpler.
class GtkGUI a where
  initialise :: IO a

-- | A GtkView simply encapsulates a GtkGUI.
data GtkGUI a => GtkView a = GtkView a

-- | Extracts the GUI from a GtkView
getGUI :: GtkGUI a => GtkView a -> a
getGUI (GtkView x) = x

-- | Instantiates the generic View for Gtk views using the default GtkGUI
-- initialiser and the default Gtk counterparts of the View class functions.
instance GtkGUI a => View (GtkView a) where
  initView   _  = initGUI >> return ()
  createView    = initialise >>= (return . GtkView)
  startView  _  = mainGUI
  onViewSync _  = postGUISync
  onViewAsync _ = postGUIAsync
  destroyView _ = mainQuit
