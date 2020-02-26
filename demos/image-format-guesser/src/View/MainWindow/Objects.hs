-- | This module contains oprations to access the objects in this interface,
-- and one to obtain a builder from which they can be accessed.
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module View.MainWindow.Objects where

import Graphics.UI.Gtk

-- | Returns a builder from which the objects in this part of the interface
-- can be accessed.
loadInterface :: IO Builder
loadInterface = do
  builder <- builderNew
  builderAddFromFile builder "Interface.glade"
  return builder

onBuilder :: (GObjectClass cls)
          => (GObject -> cls) -> String -> Builder -> IO cls
onBuilder f s b = builderGetObject b f s

-- | Returns the IDE's main window.
mainWindow :: Builder -> IO Window
mainWindow = onBuilder castToWindow "mainWindow"

-- | Returns a label to show a message to the user
mainWindowMessageLbl :: Builder -> IO Label
mainWindowMessageLbl = onBuilder castToLabel "mainWindowMessageLbl"

-- | Returns an entry to get a message to the user
mainWindowFilenameEntry :: Builder -> IO Entry
mainWindowFilenameEntry = onBuilder castToEntry "mainWindowFilenameEntry"
