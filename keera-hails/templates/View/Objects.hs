{-# LANGUAGE TemplateHaskell #-}
module View.Objects where

-- External imports
import Graphics.UI.Gtk
import Hails.MVC.View.Gtk.Builder

-- Internal imports
import Paths

loadInterface :: IO Builder
loadInterface = loadDefaultInterface getDataFileName

-- gtkBuilderAccessor element name type name
-- gtkBuilderAccessor "mainMenu"   "Menu"

-- You can use the following function to access objects obtained from the
-- Glade file.
--
-- onBuilder :: (GObjectClass cls)
--           => (GObject -> cls) -> String -> Builder -> IO cls
-- onBuilder f s b = builderGetObject b f s
--
-- Normally, the main window will be called mainWindow and you can access
-- it with the following definition.
--
-- -- | Returns the IDE's main window.
-- mainWindow :: Builder -> IO Window
-- mainWindow = onBuilder castToWindow "mainWindow"
