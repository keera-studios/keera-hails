-- | This module contains oprations to access the objects in this interface,
-- and one to obtain a builder from which they can be accessed.
module View.MainWindow.Objects where

import Graphics.UI.Gtk

-- | Returns a builder from which the objects in this part of the interface
-- can be accessed.
loadInterface :: IO Builder
loadInterface = do
  builder <- builderNew
  builderAddFromFile builder "Interface.glade"
  return builder
  
-- | Returns the IDE's main window.
mainWindow :: Builder -> IO Window
mainWindow = onBuilder castToWindow "mainWindow"

onBuilder :: (GObjectClass cls) =>
               (GObject -> cls) -> String -> Builder -> IO cls
onBuilder f s b = builderGetObject b f s
