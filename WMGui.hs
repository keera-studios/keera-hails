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
      wiimoteRV <- pollingReactive (pollWiimote wm')
                                   (Just 10000) -- Every 10ms

      -- View
      initGUI
      -- Create the builder, and load the UI file
      builder <- builderNew
      builderAddFromFile builder "UI.glade"

      -- Retrieve some objects from the UI
      window        <- builderGetObject builder castToWindow "mainWindow"
      labelA        <- builderGetObject builder castToLabel  "labelA"
      labelAccVal   <- builderGetObject builder castToLabel  "labelAccVal"
      labelPitchVal <- builderGetObject builder castToLabel  "labelPitchVal"
      labelRollVal  <- builderGetObject builder castToLabel  "labelRollVal"

      -- View: show
      widgetShowAll window

      -- Keera Hails - Reactive Controller
      (buttonA           <$> wiimoteRV) =:> liftW buttonColorF (labelBackground labelA)
      ((show.accValue)   <$> wiimoteRV) =:> labelText labelAccVal 
      ((show.pitchValue) <$> wiimoteRV) =:> labelText labelPitchVal 
      ((show.rollValue)  <$> wiimoteRV) =:> labelText labelRollVal 
      onDestroy window mainQuit

      mainGUI

-- Auxiliary functions
buttonColorF :: Bool -> Pango.Color
buttonColorF s = if s then green else defColor
  where green    = undefined
        defColor = undefined

-- Wiimote sensing
data Wiimote = Wiimote {
   buttonA     :: Bool
 , buttonB     :: Bool
 , buttonMinux :: Bool
 , buttonPlus  :: Bool
 , accValue    :: Int
 , pitchValue  :: Int
 , rollValue   :: Int
  -- ...
 }
 deriving Eq

pollWiimote :: CWiidWiimote -> IO Wiimote
pollWiimote wm = do
  [a,p,r] <- unCWiidAcc <$> cwiidGetAcc wm
  return Wiimote { buttonA     = False 
                 , buttonB     = False 
                 , buttonMinux = False 
                 , buttonPlus  = False 
                 , accValue    = a
                 , pitchValue  = p
                 , rollValue   = r
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
