{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Publishes the main elements of an label as reactive fields
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Graphics.UI.Gtk.Reactive.Label where

import Data.ReactiveValue
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Reactive.Property

labelTextReactive :: LabelClass e => e -> ReactiveFieldReadWrite IO String
labelTextReactive e = passiveProperty e labelText

instance ReactiveValueReadWrite Label String IO where

instance ReactiveValueRead Label String IO where
 reactiveValueOnCanRead = reactiveValueOnCanRead . labelTextReactive
 reactiveValueRead      = reactiveValueRead . labelTextReactive

instance ReactiveValueWrite Label String IO where
 reactiveValueWrite = reactiveValueWrite . labelTextReactive
