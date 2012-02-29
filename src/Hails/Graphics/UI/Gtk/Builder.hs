module Hails.Graphics.UI.Gtk.Builder where

import Graphics.UI.Gtk

-- | Returns a builder from which the objects in this part of the interface
-- can be accessed.
loadDefaultInterface :: (String -> IO String) -> IO Builder
loadDefaultInterface getDataFileName =
  loadInterface =<< getDataFileName "Interface.glade"

-- | Returns a builder from which the objects in this part of the interface
-- can be accessed.
loadInterface :: String -> IO Builder
loadInterface builderPath = do
  builder <- builderNew
  builderAddFromFile builder builderPath
  return builder
  
-- | Returns an element from a builder
fromBuilder :: (GObjectClass cls) =>
                (GObject -> cls) -> String -> Builder -> IO cls
fromBuilder f s b = builderGetObject b f s
