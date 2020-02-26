-- |
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Hails.MVC.View.Gtk.Builder
   (loadDefaultInterface)
  where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Extra.Builder

-- | Returns a builder from which the objects in this part of the interface
-- can be accessed.
loadDefaultInterface :: (String -> IO String) -> IO Builder
loadDefaultInterface getDataFileName =
  loadInterface =<< getDataFileName "Interface.glade"

