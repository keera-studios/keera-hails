{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Publishes the main elements of an entry as reactive fields
module Graphics.UI.Gtk.Reactive.Image where

import Data.ReactiveValue
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Reactive.Property

imageFileReactive :: Image -> ReactiveFieldWrite IO FilePath
imageFileReactive e = ReactiveFieldWrite (imageSetFromFile e)

instance ReactiveValueWrite Image String IO where
 reactiveValueWrite = reactiveValueWrite . imageFileReactive
