-- | Publishes the main elements of a scale as reactive fields
module Graphics.UI.Gtk.Reactive.Scale where

import Control.GFunctor
import Data.ReactiveValue
import GHC.Float
import Graphics.UI.Gtk

import Graphics.UI.Gtk.Reactive.Property

scaleValueReactive :: RangeClass a => a -> ReactiveFieldReadWrite IO Float
scaleValueReactive e = float_double <$$> reactiveProperty e valueChanged rangeValue
 where float_double = bijection (double2Float, float2Double)

-- ReactiveFieldReadWrite setter getter notifier
--  where getter     = fmap double2Float $ get e rangeValue
--        setter v   = set e [ rangeValue := float2Double v ]
--        notifier p = void (on e valueChanged p)
