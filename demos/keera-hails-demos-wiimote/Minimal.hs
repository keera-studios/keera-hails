module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.IfElse
import Data.ReactiveValue
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Reactive
import Hails.Polling
import System.CWiid

main :: IO ()
main = do 
   wm <- initialiseWiimote

   awhen wm $ \wm' -> do
      -- Keera Hails - External RV by polling the Wiimote
      wiimoteRV <- pollingReactive (cwiidGetAcc wm')
                                   (Just 10000) -- Every 10ms

      -- View
      initGUI
      -- View: Widgets
      window  <- windowNew
      hbox    <- hBoxNew True 10
      text1   <- entryNew
      text2   <- entryNew
      text3   <- entryNew
      -- View: layout
      set window [ windowDefaultWidth   := 200, windowDefaultHeight := 200
                 , containerBorderWidth := 10,  containerChild      := hbox
                 ]
      boxPackStart hbox text1 PackGrow 0
      boxPackStart hbox text2 PackGrow 0
      boxPackStart hbox text3 PackGrow 0
      -- View: show
      widgetShowAll window

      -- Keera Hails - Reactive Controller
      ((show.head.unCWiidAcc)  <$> wiimoteRV) =:> entryTextReactive text1
      ((show.(!!1).unCWiidAcc) <$> wiimoteRV) =:> entryTextReactive text2
      ((show.(!!2).unCWiidAcc) <$> wiimoteRV) =:> entryTextReactive text3
      onDestroy window mainQuit

      mainGUI


initialiseWiimote :: IO (Maybe CWiidWiimote)
initialiseWiimote = do
  putStrLn "Initializing WiiMote. Please press 1+2 to connect."
  wm <- cwiidOpen
  awhen wm (void . (`cwiidSetRptMode` 15)) -- Enable button reception, acc and IR
  return wm
