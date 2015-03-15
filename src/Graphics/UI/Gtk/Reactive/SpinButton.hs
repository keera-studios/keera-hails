-- | Publishes the main elements of a toggle button
module Graphics.UI.Gtk.Reactive.SpinButton where

import Control.Monad
import Graphics.UI.Gtk
import Data.ReactiveValue

spinButtonActiveReactive :: SpinButton -> ReactiveFieldReadWrite IO Int
spinButtonActiveReactive e = ReactiveFieldReadWrite setter getter notifier
 where getter   = spinButtonGetValueAsInt e
       setter   = spinButtonSetValue e . fromIntegral
       notifier = void . (onValueSpinned e)
