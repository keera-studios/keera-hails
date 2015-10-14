{-# LANGUAGE FlexibleContexts #-}
module Hails.MVC.Controller.Reactive where

-- External libraries
import Control.Monad

-- Internal libraries
import Hails.MVC.Controller.ConditionDirection
-- import Hails.MVC.Model.ReactiveModel (FullEvent (FullEvent))
import Hails.MVC.Model.ProtectedModel
import Hails.MVC.Model.ReactiveModel.Events
import Hails.MVC.View.GtkView
import Hails.MVC.Model.ProtectedModel.Reactive
import qualified Hails.MVC.GenericCombinedEnvironment as GEnv
import Graphics.UI.Gtk.GtkView

data ReactiveViewField b = ReactiveViewField
 { onChange :: IO () -> IO ()
 , rvfGet   :: IO b
 , rvfSet   :: b -> IO ()
 }

type Condition a b c = GEnv.CEnv a b c -> IO()

(=:=) :: (Eq b
         , ReactiveReadWriteField a b c e
         , InitialisedEvent e
         , GtkGUI d) =>
         ReactiveViewField b -> a -> Condition d c e
(=:=) vField mField cenv = do
  let pm = GEnv.model cenv
  onChange vField condVM
  mapM_ (\ev -> onEvent pm ev condMV) evs
 where evs    = initialisedEvent : events mField
       condMV = cond vField mField MV cenv
       condVM = cond vField mField VM cenv

cond :: (Eq b, ReactiveReadWriteField a b c e, GtkGUI d, InitialisedEvent e) => 
          ReactiveViewField b -> a -> ConditionDirection -> 
          Condition d c e
cond vField mField cd cenv = onViewAsync $ do
  let pm = GEnv.model cenv
  mShould <- rvfGet vField
  vShould <- getter mField pm
  when (mShould /= vShould) $ 
    case cd of
     MV -> rvfSet vField vShould
     VM -> setter mField pm mShould
