-- | Publishes the main elements of a menuitem
module Graphics.UI.Gtk.Reactive.Button where

import Control.Monad
import Graphics.UI.Gtk
import Data.ReactiveValue

buttonActivateField :: Button -> ReactiveFieldActivatable 
buttonActivateField b = mkActivatable op
 where op f = void (b `onClicked` f)

instance ReactiveValueActivatable Button where
  defaultActivation = buttonActivateField
