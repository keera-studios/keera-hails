{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Control.Monad
import Data.ReactiveValue
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Reactive
import Graphics.UI.Gtk.Reactive.Gtk2

main = do
   -- View
   initGUI
   window <- windowNew
   set window [windowTitle := "Text Entry", containerBorderWidth := 10]

   vb <- vBoxNew False 0
   containerAdd window vb

   txtfield <- entryNew
   boxPackStart vb txtfield PackNatural 0

   lbl <- labelNew (Nothing :: Maybe String)
   boxPackStart vb lbl PackNatural 0

   widgetShowAll window

   -- Controller Rules
   (printMsg <^> entryTextReactive txtfield) =:> labelTextReactive lbl
   objectDestroyReactive window              =:> mainQuit

   -- Run!
   mainGUI

-- Pure controller functions that can be debugged independently
printMsg ""  = ""
printMsg txt = "\"" ++ txt ++ "\" is " ++ qual ++ " to its reverse"
 where qual | txt == reverse txt = "equal"
            | otherwise          = "not equal"
