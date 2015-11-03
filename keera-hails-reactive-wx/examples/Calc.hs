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
  f     <- frameLoadRes "test.xrc" "frame_1" []

  -- Entry text
  txtT   <- entryText =<< textCtrlRes f "text_ctrl_1" []
  nBtns  <- mapM (\x -> buttonRes f ("btn" ++ show x) [] >>= buttonClick) [0..9]
  btnDot <- buttonClick =<< buttonRes f "btnDot" []

  -- Controller

  -- Numbers
  let addX :: String -> ReactiveFieldWrite IO ()
      addX x = modRW (\s _ -> s ++ x) txtT
  mapM_ (\(b,n) -> b =:> addX n -- Rule
        ) $ zip nBtns $ map show [0..9]

  -- Dot
  let addDot :: ReactiveFieldWrite IO ()
      addDot = modRW (\s () -> if '.' `elem` s then s else s ++ ".") txtT
  btnDot =:> addDot

  windowShow f
  return ()

