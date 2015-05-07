-- | Publishes the main elements of a toggle button
module Graphics.UI.Gtk.Reactive.SpinButton where

import Control.GFunctor
import Data.ReactiveValue
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Reactive.Property

spinButtonActiveReactive :: SpinButton -> ReactiveFieldReadWrite IO Int
spinButtonActiveReactive e =
  double_int <$$> reactivePropertyH e onValueSpinned spinButtonValue
 where double_int = bijection (round, fromIntegral)

spinButtonAdjustmentReactive :: SpinButton -> ReactiveFieldReadWrite IO Adjustment
spinButtonAdjustmentReactive = (`passiveProperty` spinButtonAdjustment)

-- import Control.Monad
-- spinButtonActiveReactive :: SpinButton -> ReactiveFieldReadWrite IO Int
-- spinButtonActiveReactive e = ReactiveFieldReadWrite setter getter notifier
--  where getter   = spinButtonGetValueAsInt e
--        setter   = spinButtonSetValue e . fromIntegral
--        notifier = void . (onValueSpinned e)
