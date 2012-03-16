module Hails.Graphics.UI.Gtk.Helpers.MenuItem where

import Graphics.UI.Gtk

menuItemGetLabel :: MenuItemClass self => self -> IO (Maybe Label)
menuItemGetLabel = fmap (fmap castToLabel) . binGetChild
