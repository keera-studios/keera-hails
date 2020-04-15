-- | Publishes the main elements of a toggle button
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Graphics.UI.Gtk.Reactive.SpinButton where

import Control.GFunctor
import Data.ReactiveValue
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Reactive.Property

spinButtonValueIntReactive :: SpinButton -> ReactiveFieldReadWrite IO Int
spinButtonValueIntReactive e =
  double_int <$$> reactivePropertyH e onValueSpinned spinButtonValue
 where double_int = bijection (round, fromIntegral)

spinButtonAdjustmentReactive :: SpinButton -> ReactiveFieldReadWrite IO Adjustment
spinButtonAdjustmentReactive = (`passiveProperty` spinButtonAdjustment)

spinButtonValueIntEditReactive :: SpinButton -> ReactiveFieldReadWrite IO Int
spinButtonValueIntEditReactive e =
  double_int <$$> reactivePropertyH e handler spinButtonValue
 where double_int = bijection (round, fromIntegral)
       handler = \s i -> do s `onValueSpinned` i
                            s `onEditableChanged` i

-- import Control.Monad
-- spinButtonActiveReactive :: SpinButton -> ReactiveFieldReadWrite IO Int
-- spinButtonActiveReactive e = ReactiveFieldReadWrite setter getter notifier
--  where getter   = spinButtonGetValueAsInt e
--        setter   = spinButtonSetValue e . fromIntegral
--        notifier = void . (onValueSpinned e)
