module Hails.Graphics.UI.Gtk.Helpers.MessageDialog where

import Graphics.UI.Gtk
import Control.Monad

popupError :: String -> String -> IO ()
popupError t s = do
  md <- messageDialogNew Nothing [DialogModal] MessageError ButtonsOk s
  set md [ windowTitle := t ]
  void $ dialogRun md >> widgetDestroy md
