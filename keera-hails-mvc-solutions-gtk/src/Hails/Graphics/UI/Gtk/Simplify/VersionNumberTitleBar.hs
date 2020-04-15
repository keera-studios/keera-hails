{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}
-- |
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Hails.Graphics.UI.Gtk.Simplify.VersionNumberTitleBar where

-- External imports
import Control.Arrow
import Control.Monad
import Control.Monad.Reader (liftIO)
import Data.ExtraVersion
import Graphics.UI.Gtk
-- import Graphics.UI.Gtk.GenericView ()
import Hails.MVC.View
import Hails.MVC.View.GtkView
import Hails.MVC.GenericCombinedEnvironment
import Hails.MVC.Model.ReactiveModel (Event)
import Hails.MVC.Model.ProtectedModel

-- Internal imports
import Data.ReactiveValue
import Hails.MVC.Model.ProtectedModel.VersionedModel

installHandlers :: (GtkGUI a, VersionedBasicModel b, Event c) => CEnv a b c
                -> ViewElementAccessorIO (GtkView a) Window
                -> IO ()
installHandlers cenv wF = void $ do
  let (vw, pm) = (view &&& model) cenv
  w <- wF vw
  w `on` mapEvent $ liftIO (condition cenv wF) >> return False

  -- -- let l f = do _ <- w `on` mapEvent $ liftIO f >> return False
  -- --              return ()
  -- let -- v1 :: (GtkGUI a, VersionedBasicModel b) => TypedReactiveValue (ProtectedModelVersion a b) String
  --     v1 = TypedReactiveValue (ProtectedModelVersion pm w) (undefined :: String)
  --     -- v2 :: TypedReactiveValue Window String
  --     v2 = TypedReactiveValue w (undefined :: String)
  -- v1 =:> v2

-- type WindowTitle = TypedReactiveValue Window String
-- 
-- instance ReactiveValueWrite Window String where
--   reactiveValueWrite w v = do
--     t  <- windowGetTitle w
--     when (t /= v) $ windowSetTitle w v
-- 
-- data ProtectedModelVersion a b =
--   ProtectedModelVersion (ProtectedModel a b) Window
-- 
-- type ProgramVersion a b = TypedReactiveValue (ProtectedModelVersion a b) String
-- 
-- instance (VersionedBasicModel b, Event c) => ReactiveValueRead (ProtectedModelVersion b c) String where
--   reactiveValueOnCanRead (ProtectedModelVersion _ w) _ op = do
--     _ <- do w `on` mapEvent $ liftIO op >> return False
--     return ()
--   reactiveValueRead (ProtectedModelVersion pm _) = do
--     fmap versionToString $ getVersion pm

condition :: (GtkGUI a, VersionedBasicModel b, Event c) => CEnv a b c
          -> ViewElementAccessorIO (GtkView a) Window
          -> IO ()
condition cenv wF = do
  let (vw, pm) = (view &&& model) cenv
  w  <- wF vw
  vn <- fmap versionToString $ getVersion pm
  t  <- windowGetTitle w
  when (t /= vn) $ windowSetTitle w vn
