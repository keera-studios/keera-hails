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
      labelB        <- builderGetObject builder castToLabel  "labelB"
      label1        <- builderGetObject builder castToLabel  "label1"
      label2        <- builderGetObject builder castToLabel  "label2"
      labelLeft     <- builderGetObject builder castToLabel  "labelLeft"
      labelRight    <- builderGetObject builder castToLabel  "labelRight"
      labelUp       <- builderGetObject builder castToLabel  "labelUp"
      labelDown     <- builderGetObject builder castToLabel  "labelDown"
      labelHome     <- builderGetObject builder castToLabel  "labelHome"
      labelPlus     <- builderGetObject builder castToLabel  "labelPlus"
      labelMinus    <- builderGetObject builder castToLabel  "labelMinus"
      labelAccVal   <- builderGetObject builder castToLabel  "labelAccVal"
      labelPitchVal <- builderGetObject builder castToLabel  "labelPitchVal"
      labelRollVal  <- builderGetObject builder castToLabel  "labelRollVal"

      -- View: show
      widgetShowAll window

      -- Keera Hails - Reactive Controller

      -- Controller: synchronise buttons
      ((buttonColorF.buttonA)     <$> wiimoteRV) =:> labelBackground labelA
      ((buttonColorF.buttonB)     <$> wiimoteRV) =:> labelBackground labelB
      ((buttonColorF.button1)     <$> wiimoteRV) =:> labelBackground label1
      ((buttonColorF.button2)     <$> wiimoteRV) =:> labelBackground label2
      ((buttonColorF.buttonLeft)  <$> wiimoteRV) =:> labelBackground labelLeft
      ((buttonColorF.buttonRight) <$> wiimoteRV) =:> labelBackground labelRight
      ((buttonColorF.buttonUp)    <$> wiimoteRV) =:> labelBackground labelUp
      ((buttonColorF.buttonDown)  <$> wiimoteRV) =:> labelBackground labelDown
      ((buttonColorF.buttonHome)  <$> wiimoteRV) =:> labelBackground labelHome
      ((buttonColorF.buttonPlus)  <$> wiimoteRV) =:> labelBackground labelPlus
      ((buttonColorF.buttonMinus) <$> wiimoteRV) =:> labelBackground labelMinus

      -- Controller: synchronise accelerometre data
      ((show.accValue)            <$> wiimoteRV) =:> labelText labelAccVal 
      ((show.pitchValue)          <$> wiimoteRV) =:> labelText labelPitchVal 
      ((show.rollValue)           <$> wiimoteRV) =:> labelText labelRollVal 
      onDestroy window mainQuit

      -- Controller: main loop
      mainGUI

-- Auxiliary functions

-- TODO: This may not be the default color. It's theme dependent.
-- There must be some function to obtain it.
buttonColorF :: Bool -> Pango.Color
buttonColorF s = if s then green else defColor
  where green    = Pango.Color 0 65535 0
        defColor = Pango.Color 32768 32768 32768  -- Neutral gray

-- * Wiimote

-- | This datatype overlaps with CWiidState.
--   TODO: CWiidState is incomplete.
data Wiimote = Wiimote {
   buttonA     :: Bool
 , buttonB     :: Bool
 , buttonMinus :: Bool
 , buttonPlus  :: Bool
 , buttonLeft  :: Bool
 , buttonRight :: Bool
 , buttonUp    :: Bool
 , buttonDown  :: Bool
 , buttonHome  :: Bool
 , button1     :: Bool
 , button2     :: Bool
 , accValue    :: Int
 , pitchValue  :: Int
 , rollValue   :: Int
  -- ...
 }
 deriving Eq

-- | Poll the wiimote, refresh every field
--
-- TODO: This function belongs in Hcwiid.
pollWiimote :: CWiidWiimote -> IO Wiimote
pollWiimote wm = do
  [a,p,r] <- unCWiidAcc <$> cwiidGetAcc wm
  enabledBtns <- cwiidGetBtnState wm
  return Wiimote { buttonA     = cwiidIsBtnPushed enabledBtns cwiidBtnA 
                 , buttonB     = cwiidIsBtnPushed enabledBtns cwiidBtnB
                 , buttonMinus = cwiidIsBtnPushed enabledBtns cwiidBtnMinus
                 , buttonPlus  = cwiidIsBtnPushed enabledBtns cwiidBtnPlus
                 , buttonLeft  = cwiidIsBtnPushed enabledBtns cwiidBtnLeft
                 , buttonRight = cwiidIsBtnPushed enabledBtns cwiidBtnRight
                 , buttonUp    = cwiidIsBtnPushed enabledBtns cwiidBtnUp
                 , buttonDown  = cwiidIsBtnPushed enabledBtns cwiidBtnDown
                 , buttonHome  = cwiidIsBtnPushed enabledBtns cwiidBtnHome
                 , button1     = cwiidIsBtnPushed enabledBtns cwiidBtn1
                 , button2     = cwiidIsBtnPushed enabledBtns cwiidBtn2
                 , accValue    = a
                 , pitchValue  = p
                 , rollValue   = r
                 }

-- | Connect to a wiimote. Enable button reception, acc and IR
initialiseWiimote :: IO (Maybe CWiidWiimote)
initialiseWiimote = do
  putStrLn "Initializing WiiMote. Please press 1+2 to connect."
  wm <- cwiidOpen
  awhen wm (void . (`cwiidSetRptMode` 15))
  return wm

-- * Keera Hails
labelBackground :: Label -> ReactiveFieldReadWrite Pango.Color
labelBackground lbl = ReactiveFieldReadWrite setter getter (const (return ())
 where setter x = labelSetAttributes lbl [AttrBackground 0 (-1) x)]
       getter   = (\ls -> head [ x | AttrBackground _ _ x <- ls])
                  <$> labelGetAttributes lbl
