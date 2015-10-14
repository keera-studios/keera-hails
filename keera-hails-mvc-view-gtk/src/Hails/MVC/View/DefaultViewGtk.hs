-- | Contains a basic UI definition
--
module Hails.MVC.View.DefaultViewGtk where

-- External libraries
import Graphics.UI.Gtk

import Hails.MVC.View.GladeView

-- | This datatype should hold the elements that we must track in the
-- future (for instance, treeview models)
data View = View
  { uiBuilder :: Builder }

instance GladeView View where
  ui = uiBuilder
