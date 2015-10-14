module Hails.MVC.View.Reactive
  ( Accessor
  , Setter
  , Getter
  )
 where

-- External libraries
import Graphics.UI.Gtk

-- Internal libraries
import Hails.MVC.Model.ProtectedModel.Reactive

type Accessor a = Builder -> IO a
