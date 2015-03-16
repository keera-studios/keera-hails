-- NOTE: This module is work in progress, incomplete and contains errors.
module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.IfElse
import Data.ReactiveValue
import Graphics.Rendering.Pango as Pango
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
      -- Create the builder, and load the UI file
      builder <- builderNew
      builderAddFromFile builder "UI.glade"

      -- Retrieve some objects from the UI
      window <- builderGetObject builder castToWindow "mainWindow"
      labelA <- builderGetObject builder castToLabel  "labelA"

      -- View: show
      widgetShowAll window

      -- Keera Hails - Reactive Controller
      (buttonA <$> wiimoteRV) =:> liftW buttonColorF (labelBackground labelA)
      ((show.head.unCWiidAcc)  <$> wiimoteRV) =:> entryTextReactive text1
      ((show.(!!1).unCWiidAcc) <$> wiimoteRV) =:> entryTextReactive text2
      ((show.(!!2).unCWiidAcc) <$> wiimoteRV) =:> entryTextReactive text3
      onDestroy window mainQuit

      mainGUI

-- Auxiliary functions
buttonColorF :: Bool -> Pango.Color
buttonColorF = undefined

-- Wiimote sensing
data Wiimote = Wiimote {
   buttonA     :: Bool
 , buttonB     :: Bool
 , buttonMinux :: Bool
 , buttonPlus  :: Bool
 , rollValue   :: Int
 , pitchValue  :: Int
 , accValue    :: Int
  -- ...
 }
 deriving Eq

pollWiimote :: CWiidWiimote -> IO Wiimote
pollWiimote wm = do
  [a,p,r] <- cwiidGetAcc wm
  return Wiimote { buttonA     = False 
                 , buttonB     = False 
                 , buttonMinux = False 
                 , buttonPlus  = False 
                 , rollValue   = r
                 , pitchValue  = p
                 , accValue    = a
                 }

initialiseWiimote :: IO (Maybe CWiidWiimote)
initialiseWiimote = do
  putStrLn "Initializing WiiMote. Please press 1+2 to connect."
  wm <- cwiidOpen
  awhen wm (void . (`cwiidSetRptMode` 15)) -- Enable button reception, acc and IR
  return wm

-- Keera Hails
labelBackground :: Label -> ReactiveFieldReadWrite Pango.Color
labelBackground = undefined
