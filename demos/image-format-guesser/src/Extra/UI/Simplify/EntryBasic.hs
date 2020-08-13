-- |
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Extra.UI.Simplify.EntryBasic
    (installHandlers)
  where

-- External libraries
import Control.Arrow
import Control.Monad
import Data.Maybe
import Graphics.UI.Gtk
import Hails.MVC.View         (ViewElementAccessorIO)
import Hails.MVC.View.GtkView hiding (View, onViewAsync)

-- Internal libraries
import CombinedEnvironment
import Controller.Conditions.ConditionDirection
import View
import Model.ProtectedModel

type Accessor a = ViewElementAccessorIO View a
type Setter a   = ProtectedModel -> a -> IO()
type Getter a   = ProtectedModel -> IO a

installHandlers :: [ ModelEvent ] -> Accessor Entry -> Setter String -> Getter (Maybe String) -> CEnv -> IO()
installHandlers evs entryF setter getter cenv = do
  let (vw, pm) = (view &&& model) cenv
  entry  <- entryF vw
  entry `onEditableChanged` condition cenv VM entryF setter getter
  mapM_ (\ev -> onEvent pm ev (condition cenv MV entryF setter getter)) evs

-- | Enforces the condition
condition :: CEnv -> ConditionDirection -> Accessor Entry -> Setter String -> Getter (Maybe String) ->IO()
condition cenv cd entryF setter getter = onViewAsync $ do
  let (vw, pm) = (view &&& model) cenv
  entry            <- entryF vw
  t                <- get entry entryText
  curModelValue    <- getter pm
  let viewValueShould = fromMaybe "" curModelValue

  case cd of
   MV -> when (viewValueShould /= t) $
           set entry [ entryText := viewValueShould ]
   VM -> when (curModelValue /= Just t) $ setter pm t
