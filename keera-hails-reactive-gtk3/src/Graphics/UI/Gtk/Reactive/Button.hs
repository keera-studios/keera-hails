{-# LANGUAGE MultiParamTypeClasses #-}
-- | Publishes the main elements of a menuitem
module Graphics.UI.Gtk.Reactive.Button where

import Control.Monad
import Graphics.UI.Gtk
import Data.ReactiveValue

buttonActivateField :: Button -> ReactiveFieldActivatable IO
buttonActivateField b = mkActivatable op
 where op f = void (on b buttonActivated f)

instance ReactiveValueActivatable IO Button where
  defaultActivation = buttonActivateField
