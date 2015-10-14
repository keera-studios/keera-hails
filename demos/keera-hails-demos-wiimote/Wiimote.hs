-- | TODO: Move most of this to hcwiid
module Wiimote where

import Control.Applicative
import Control.Monad
import Control.Monad.IfElse
import Data.Maybe
import System.CWiid

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
 , irData      :: [(Int, Int, Int)]
  -- ...
 }
 deriving Eq

-- | Poll the wiimote, refresh every field
--
-- TODO: This function belongs in Hcwiid.
pollWiimote :: CWiidWiimote -> IO Wiimote
pollWiimote wm = do
  [a,p,r] <- unCWiidAcc <$> cwiidGetAcc wm
  irs     <- (catMaybes . map irCircle) <$> cwiidGetIR wm
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
                 , irData      = irs
                 }

-- | Connect to a wiimote. Enable button reception, acc and IR
initialiseWiimote :: IO (Maybe CWiidWiimote)
initialiseWiimote = do
  putStrLn "Initializing WiiMote. Please press 1+2 to connect."
  wm <- cwiidOpen
  awhen wm (void . (`cwiidSetRptMode` 15))
  return wm

irCircle :: CWiidIRSrc -> Maybe (Int, Int, Int)
irCircle ir
  | cwiidIRSrcValid ir = Just ( cwiidIRSrcPosX ir
                              , cwiidIRSrcPosY ir
                              , cwiidIRSrcSize ir
                              )
  | otherwise          = Nothing
