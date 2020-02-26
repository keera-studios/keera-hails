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
import Graphics.UI.View hiding (View, onViewAsync)
import Graphics.UI.Gtk.GenericView

-- Internal libraries
import CombinedEnvironment
import Controller.Conditions.ConditionDirection
import View
import Model.ProtectedModel

type Accessor a = ViewElementAccessor' View a
type Setter a   = ProtectedModel -> a -> IO()
type Getter a   = ProtectedModel -> IO a

installHandlers :: [ ModelEvent ] -> Accessor Entry -> Setter String -> Getter (Maybe String) -> CRef -> IO()
installHandlers evs entryF setter getter cref = do
  (vw, pm) <- fmap (view &&& model) $ readIORef cref
  entry  <- entryF vw
  entry `onEditableChanged` condition cref VM entryF setter getter
  mapM_ (\ev -> onEvent pm ev (condition cref MV entryF setter getter)) evs

-- | Enforces the condition
condition :: CRef -> ConditionDirection -> Accessor Entry -> Setter String -> Getter (Maybe String) ->IO()
condition cref cd entryF setter getter = onViewAsync $ do
  (vw, pm) <- fmap (view &&& model) $ readIORef cref
  entry            <- entryF vw
  t                <- get entry entryText
  curModelValue    <- getter pm
  let viewValueShould = fromMaybe "" curModelValue

  case cd of
   MV -> when (viewValueShould /= t) $
           set entry [ entryText := viewValueShould ]
   VM -> when (curModelValue /= Just t) $ setter pm t
