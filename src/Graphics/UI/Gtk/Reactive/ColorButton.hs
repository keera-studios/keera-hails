{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Publishes the main elements of an entry as reactive fields
module Graphics.UI.Gtk.Reactive.ColorButton where

import Control.Monad (void, when)
import Graphics.UI.Gtk
import Data.ReactiveValue
import Data.Word

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

instance ReactiveValueReadWrite ColorButton Color4 IO where

instance ReactiveValueRead ColorButton Color4 IO where
 reactiveValueOnCanRead = reactiveValueOnCanRead . colorButtonColorReactive 
 reactiveValueRead      = reactiveValueRead . colorButtonColorReactive

instance ReactiveValueWrite ColorButton Color4 IO where
 reactiveValueWrite = reactiveValueWrite . colorButtonColorReactive
