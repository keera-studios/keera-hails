-- |
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Hails.Graphics.UI.Gtk.Simplify.RootLogger
    (installHandlers, installHandlersUnique)
  where

import Control.Monad (void)
import Control.Monad.Reader (liftIO)
import Data.Maybe
-- import GenericCombinedEnvironment
import Graphics.UI.Gtk
-- import Graphics.UI.Gtk.GenericView
import Hails.MVC.GenericCombinedEnvironment
import Hails.MVC.View.GtkView
import Hails.MVC.View
import Hails.MVC.Model.ReactiveModel (Event)
import System.Log as Log
import System.Log.Formatter
import System.Log.Handler
import System.Log.Logger

installHandlersUnique :: (GtkGUI a,
                          Event c, MenuItemClass d)
                      => CEnv a b c
                      -> ViewElementAccessorIO (GtkView a) d
                      -> IO ()
installHandlersUnique cenv mF = void $ do
  rl <- getRootLogger
  let lhs = [] :: [ListStoreLogHandler]
  let rl' = setHandlers lhs rl
  saveGlobalLogger rl'
  installHandlers cenv mF

installHandlers :: (GtkGUI a,
                    Event c, MenuItemClass d)
                => CEnv a b c
                -> ViewElementAccessorIO (GtkView a) d
                -> IO ()
installHandlers cenv mF = void $ do
  let vw = view cenv

  lsLogHandler <- listStoreLogHandlerNew
  rl <- getRootLogger
  let nl = addHandler lsLogHandler rl
  saveGlobalLogger nl

  w  <- createLogWindow $ lslhStore lsLogHandler
  mn <- mF vw
  mn `on` menuItemActivate $ liftIO (widgetShowAll w)

createLogWindow :: ListStore String -> IO Window
createLogWindow ls = do
  w <- windowNew
  set w [ windowTitle := "Log" ]
  windowSetDefaultSize w 400 300
  s <- scrolledWindowNew Nothing Nothing
  containerAdd w s
  tv <- treeViewNewWithModel ls
  treeViewSetHeadersVisible tv False
  addTextColumn tv ls Just
  containerAdd s tv
  w `on` deleteEvent $ liftIO $ widgetHide w >> return True
  return w -- , ls)

addTextColumn :: (TreeModelClass (model row), TypedTreeModelClass model)
                => TreeView -> model row -> (row -> Maybe String) -> IO()
addTextColumn tv st f = do
  col <- treeViewColumnNew
  renderer <- cellRendererTextNew
  cellLayoutPackStart col renderer True
  cellLayoutSetAttributes col renderer st $ map (cellText :=).maybeToList.f
  _ <- treeViewAppendColumn tv col
  return ()

data ListStoreLogHandler = ListStoreLogHandler
 { lslhStore     :: ListStore String
 , lslhLevel     :: Log.Priority
 , lslhFormatter :: LogFormatter ListStoreLogHandler
 }

instance LogHandler ListStoreLogHandler where
 getLevel         = lslhLevel
 setLevel x l     = x { lslhLevel = l }
 getFormatter     = lslhFormatter
 setFormatter x f = x { lslhFormatter = f }
 emit x l _       = void (listStoreAppend (lslhStore x) (snd l))
 close _          = return ()

listStoreLogHandlerNew :: IO ListStoreLogHandler
listStoreLogHandlerNew = do
  ls <- listStoreNew []
  return ListStoreLogHandler
           { lslhStore     = ls
           , lslhLevel     = DEBUG
           , lslhFormatter = nullFormatter
           }
