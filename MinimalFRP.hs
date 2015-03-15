module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.IfElse
import Data.ReactiveValue
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Reactive
import Hails.Polling
import Hails.Yampa
import System.CWiid
import FRP.Yampa

main :: IO ()
main = do 
   wm <- initialiseWiimote

   awhen wm $ \wm' -> do
      -- Keera Hails - External RV by polling the Wiimote
      wiimoteRV <- pollingReactive (cwiidGetAcc wm')
                                   (Just 10000) -- Every 10ms

      let integrateAcc :: SF Int Float
          integrateAcc = arr ((-) 128) >>> arr fromIntegral >>> integral
                         
      (wX,rX) <- yampaReactiveDual 0 integrateAcc
      (wY,rY) <- yampaReactiveDual 0 integrateAcc
      (wZ,rZ) <- yampaReactiveDual 0 integrateAcc

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
      -- Integrate X
      ((head.unCWiidAcc)  <$> wiimoteRV) =:> wX
      (show <$> rX)                      =:> entryTextReactive text1

      -- Integrate Y
      (((!!1).unCWiidAcc)  <$> wiimoteRV) =:> wY
      (show <$> rY)                       =:> entryTextReactive text2

      -- Integrate Y
      (((!!2).unCWiidAcc)  <$> wiimoteRV) =:> wZ
      (show <$> rZ)                       =:> entryTextReactive text3

      onDestroy window mainQuit

      mainGUI


initialiseWiimote :: IO (Maybe CWiidWiimote)
initialiseWiimote = do
  putStrLn "Initializing WiiMote. Please press 1+2 to connect."
  wm <- cwiidOpen
  awhen wm (void . (`cwiidSetRptMode` 15)) -- Enable button reception, acc and IR
  return wm
