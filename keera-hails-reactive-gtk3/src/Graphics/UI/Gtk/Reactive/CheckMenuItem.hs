-- | Publishes the main elements of a checkmenuitem
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Graphics.UI.Gtk.Reactive.CheckMenuItem where

import Control.Monad
import Graphics.UI.Gtk
import Data.ReactiveValue

checkMenuItemActiveReactive :: CheckMenuItem -> ReactiveFieldReadWrite IO Bool
checkMenuItemActiveReactive e = ReactiveFieldReadWrite setter getter notifier
 where getter   = checkMenuItemGetActive e
       setter   = checkMenuItemSetActive e
       notifier = void . (on e checkMenuItemToggled)
