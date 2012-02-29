-- | Contains basic operations related to the GUI
--
-- | FIXME: In a very rails-like move, this module will likely be
-- exactly the same for all programs, so we should try to put
-- a "Convention-over-configuration" policy in place and remove this
-- unless it must be adapted by the user.
module Hails.MVC.View.DefaultView where

-- External libraries
import Graphics.UI.Gtk

-- | This datatype should hold the elements that we must track in the
-- future (for instance, treeview models)
data View = View
  { uiBuilder :: Builder }
