-- |
--
-- Copyright   : (C) Keera Studios Ltd, 2013-2020
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
--
module Controller.Conditions.Width where

import Control.Concurrent
import Control.Monad                           (forever)
import Control.Monad.IfElse                    (returning)
import Data.CBRef
import Data.Maybe                              (fromMaybe)
import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.InternalCore
import Data.ReactiveValue
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk                         hiding (rectangle)
import Hails.MVC.Model.ProtectedModel.Reactive
import Hails.MVC.View                          (onViewAsync)

import CombinedEnvironment
import Model.Model
import View
import View.Objects

installHandlers :: CEnv -> IO()
installHandlers cenv = do
   -- View listeners
   ly <- mainDrawingArea $ uiBuilder $ view cenv
   ly `on` exposeEvent $ (`returning` True) $ \_ -> liftIO hndlr

   -- Model listeners
   onEvents (model cenv) evs hndlr

   let reactiveModelWidthField = mkFieldAccessor modelWidthField (model cenv)
   (dw, dr) <- dunaiRW testMSF

   (fromMaybe 0 <$> dr) =:> reactiveModelWidthField

   forkIO $ forever $ do
     threadDelay 10000
     reactiveValueWrite dw ()

   return ()

 where

   hndlr = drawOp cenv
   evs = [ ModelWidthChanged ]

drawOp :: CEnv -> IO()
drawOp cenv = onViewAsync (GtkView (view cenv)) $ do
  ly    <- widgetGetDrawWindow =<< mainDrawingArea (uiBuilder $ view cenv)
  regio <- regionRectangle $ Rectangle 0 0 300 300

  let reactiveModelWidthField = mkFieldAccessor modelWidthField (model cenv)

  -- Rule:
  i <- reactiveValueRead reactiveModelWidthField

  print i
  drawWindowBeginPaintRegion ly regio
  renderWithDrawable ly $ do
    -- White background
    setSourceRGBA 0.8 0.8 0.8 1 -- white
    rectangle 0 0 300 300
    fill

    setLineWidth 2
    setDash [1, 1] 1
    setSourceRGBA 0 0 0 1.0
    uncurry moveTo (10, 10)
    uncurry lineTo (10 + fromIntegral i, 10 + fromIntegral i)
    stroke

  drawWindowEndPaint ly

testMSF :: MSF IO () Int
testMSF = feedback 0 $ arrM $ \((), n) -> do
  let n' = (n + 1) `mod` 256
  return (n', n')

dunaiRW :: MSF IO a b -> IO (ReactiveFieldWrite IO a, ReactiveFieldRead IO (Maybe b))
dunaiRW msf = do
  msfRef <- newCBRef (msf, Nothing)

  let actionW a = do
        (msf', _) <- readCBRef msfRef
        (b, msf'') <- unMSF msf' a
        writeCBRef msfRef (msf'', Just b)

      actionR = snd <$> readCBRef msfRef

  return (wrapMW actionW, wrapMR actionR (installCallbackCBRef msfRef))
