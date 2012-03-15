-- | Contains basic operations related to the GUI
--
module View (module Exported) where

-- External libraries
import Graphics.UI.Gtk.GtkView (GtkGUI(..))

-- Internal libraries
import Hails.MVC.View.GtkView     as Exported
import Hails.MVC.View.DefaultView as Exported
import View.MainWindow.Objects

-- | Add all initialisers to the initialise operation and store
-- everything we'll need in the view. We need this operation here
-- because the URL to the glade file depends on the application
-- name.
instance GtkGUI View where
  initialise = fmap View loadInterface
