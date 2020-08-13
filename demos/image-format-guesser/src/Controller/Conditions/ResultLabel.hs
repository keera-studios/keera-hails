-- |
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Controller.Conditions.ResultLabel where

import Control.Arrow
import qualified Control.Exception as E
import Data.Maybe
import Graphics.GD as GD
import Graphics.UI.Gtk
import System.Directory

-- Internal libraries
import CombinedEnvironment
import View
import View.MainWindow.Objects
import Model.ProtectedModel

installHandlers :: CEnv -> IO()
installHandlers cenv = do
  let pm = model cenv
  onEvent pm FilenameChanged $ condition cenv

condition :: CEnv -> IO()
condition cenv = do
  let (v, pm) = (view &&& model) cenv
  lbl <- mainWindowMessageLbl $ mainWindowBuilder v
  fn  <- getFilename pm
  exists <- doesFileExist fn
  if exists
   then do f <- getImageType fn
           set lbl [ labelLabel := show f ]
   else set lbl [ labelLabel := "unknown file" ]

getImageType :: String -> IO (Maybe Format)
getImageType s =
 untilSuccess [ loadJpegFile s >> return JPEG
              , loadPngFile s  >> return PNG
              , loadGifFile s  >> return GIF
              ]

loadFile :: String -> IO (Maybe GD.Image)
loadFile s =
 untilSuccess [ loadJpegFile s
              , loadPngFile s
              , loadGifFile s
              ]

safeDo :: IO a -> IO (Maybe a)
safeDo f = E.catch (fmap Just f) handler
 where handler :: E.SomeException -> IO (Maybe a)
       handler _ = return Nothing

untilSuccess :: [IO a] -> IO (Maybe a)
untilSuccess [] = return Nothing
untilSuccess (f:fs) = do
 r <- safeDo f
 if isNothing r
  then untilSuccess fs
  else return r

data Format = GIF | JPEG | PNG
 deriving Show
