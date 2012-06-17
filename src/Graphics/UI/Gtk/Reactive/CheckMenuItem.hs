-- | Publishes the main elements of a checkmenuitem
module Graphics.UI.Gtk.Reactive.CheckMenuItem where

import Control.Monad
import Graphics.UI.Gtk
import Data.ReactiveValue

checkMenuItemActiveReactive :: CheckMenuItem -> ReactiveFieldReadWrite Bool
checkMenuItemActiveReactive e = ReactiveFieldReadWrite setter getter notifier
 where getter   = checkMenuItemGetActive e
       setter   = checkMenuItemSetActive e
       notifier = void . (on e checkMenuItemToggled)
