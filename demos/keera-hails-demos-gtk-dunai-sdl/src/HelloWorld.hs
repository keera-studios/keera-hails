-- |
--
-- Copyright   : (C) Keera Studios Ltd, 2020
-- License     : BSD3
-- Maintainer  : support@keera.co.uk

{-# LANGUAGE Arrows #-}
import Control.Concurrent
import Control.Monad                           (forever)
import Control.Monad.Trans.MSF.Reader
import Data.IORef
import Data.Maybe
import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.InternalCore
import Data.ReactiveValue
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Reactive
import Graphics.UI.Gtk.Reactive.Gtk2
import Graphics.UI.SDL                         as SDL

main = do
   -- View
   initGUI
   window <- windowNew
   set window [windowTitle := "Text Entry", containerBorderWidth := 10]

   ref <- newIORef 0

   start <- dunaiBallIO ref

   -- RV sink that executes one step of an MSF
   let dunaiRV = wrapMW (writeIORef ref)

   vb <- vBoxNew False 0
   containerAdd window vb

   txtfield <- spinButtonNewWithRange 0 254 10
   boxPackStart vb txtfield PackNatural 0

   lbl <- labelNew (Nothing :: Maybe String)
   boxPackStart vb lbl PackNatural 0

   widgetShowAll window

   let button = spinButtonValueIntReactive txtfield
   -- Controller Rules
   (show <^> button) =:> labelTextReactive lbl
   objectDestroyReactive window              =:> mainQuit
   button =:> dunaiRV

   -- Run!
   mainGUI

myMSF :: Monad m => MSF (ReaderT String m) () Int
myMSF = constM ask >>> arr length

-- Pure controller functions that can be debugged independently
printMsg ""  = ""
printMsg txt = "\"" ++ txt ++ "\" is " ++ qual ++ " to its reverse"
 where qual | txt == reverse txt = "equal"
            | otherwise          = "not equal"

dunaiWO :: MSF IO a () -> IO (ReactiveFieldWrite IO a)
dunaiWO msf = do
  msfRef <- newIORef msf
  return $ wrapMW $ \x -> do
             msf' <- readIORef msfRef
             ((), msf'') <- unMSF msf' x
             writeIORef msfRef msf''

dunaiBallIO ioref = do
  m <- dunaiBall
  let m' = runReaderS m
  let go msf = do
        x <- readIORef ioref
        ((), msf') <- unMSF msf (x, ())
        go msf'
  forkIO $ go m'

dunaiBall :: IO (MSF (ReaderT Int IO) () ())
dunaiBall = do
    SDL.init [InitVideo]
    screen <- SDL.setVideoMode 500 400 32 [SWSurface]
    let format = surfaceGetPixelFormat screen
    return $ proc () -> do
      t <- constM ask -< ()
      liftBaseM (action screen format) -< t
  where
    action :: Surface -> PixelFormat -> Int -> IO ()
    action screen format b = do
      putStrLn "."
      blue <- SDL.mapRGB format (fromIntegral b `mod` 255) 0 0xFF
      SDL.fillRect screen Nothing blue
      SDL.flip screen
      threadDelay 10

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
                [(x, "")] -> Just x
                _         -> Nothing
