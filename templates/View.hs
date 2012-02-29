-- | Contains basic operations related to the GUI
--
-- | FIXME: In a very rails-like move, this module will likely be
-- exactly the same for all programs, so we should try to put
-- a "Convention-over-configuration" policy in place and remove this
-- unless it must be adapted by the user.
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