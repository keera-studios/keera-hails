module Main where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Control.GFunctor
import Data.ReactiveValue
import Graphics.UI.WX as WX
import Graphics.UI.WXCore as WXCore
import Graphics.UI.WX.Reactive

main :: IO ()
main = start $ do 
  -- View
  f     <- frame    [text := "Hello!"]
  btn   <- button f [text := "Quit"]
  txt1  <- entry  f [text := ""]
  txt2  <- entry  f [text := ""]
  txt1T <- entryText txt1
  txt2T <- entryText txt2
  btnC  <- buttonClick btn
  set f [layout := margin 20 $
               floatCentre $
               column 3 [widget txt1, widget txt2, widget btn]]

  -- Controller
  (involution reverse <$$> txt1T) =:= txt2T
  (const "a" <$> btnC) =:> txt2T
