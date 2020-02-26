{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Publishes the main elements of an entry as reactive fields
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Graphics.UI.Gtk.Reactive.ColorButton where

import Control.Monad (void, when)
import Graphics.UI.Gtk
import Data.ReactiveValue
import Data.Word
import Graphics.UI.Gtk.Reactive.Property

type Color4 = (Word16, Word16, Word16, Word16)

colorButtonColorReactive :: ColorButton -> ReactiveFieldReadWrite IO Color4
colorButtonColorReactive e = ReactiveFieldReadWrite setter getter notifier
 where getter     = do (Color r g b) <- colorButtonGetColor e
                       alpha         <- colorButtonGetAlpha e
                       return (r, g, b, alpha)

       setter c@(r,g,b,a) = postGUIAsync $ do
                              c' <- getter
                              when (c /= c') $ do
                                colorButtonSetColor e (Color r g b)
                                colorButtonSetAlpha e a
       notifier p = void (e `onColorSet` p)

colorButtonColorReactive' :: ColorButton -> ReactiveFieldReadWrite IO Color4
colorButtonColorReactive' e = liftRW2 color4_colorAlpha (colorButtonRGBReactive e) (colorButtonAlphaReactive e)

colorButtonRGBReactive :: ColorButton -> ReactiveFieldReadWrite IO Color
colorButtonRGBReactive e = eqCheck $ ReactiveFieldReadWrite
  (colorButtonSetColor e) (colorButtonGetColor e) (void.onColorSet e)

colorButtonAlphaReactive :: ColorButton -> ReactiveFieldReadWrite IO Word16
colorButtonAlphaReactive e = reactivePropertyH e onColorSet colorButtonAlpha

color4_colorAlpha :: BijectiveFunc Color4 (Color, Word16)
color4_colorAlpha = bijection (\(r,g,b,a) -> (Color r g b, a), \(Color r g b, a) -> (r,g,b,a))

instance ReactiveValueReadWrite ColorButton Color4 IO where

instance ReactiveValueRead ColorButton Color4 IO where
 reactiveValueOnCanRead = reactiveValueOnCanRead . colorButtonColorReactive
 reactiveValueRead      = reactiveValueRead . colorButtonColorReactive

instance ReactiveValueWrite ColorButton Color4 IO where
 reactiveValueWrite = reactiveValueWrite . colorButtonColorReactive
