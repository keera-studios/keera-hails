-----------------------------------------------------------------------------
{-| Program   : widgets.hs
    Copyright : (c) David Harley 2010-
                (c) Ivan Perez 2015-
--}
-----------------------------------------------------------------------------

module Main where

-- Keera Hails - Reactive Values
import Control.GFunctor
import Data.ReactiveValue
import Graphics.UI.Qt.Reactive

-- Qt
import Qtc.Classes.Base
import Qtc.Classes.Gui
import Qtc.ClassTypes.Gui
import Qtc.Core.Base
import Qtc.Gui.Base
import Qtc.Gui.QApplication
import Qtc.Gui.QGridLayout
import Qtc.Gui.QGroupBox
import Qtc.Gui.QVBoxLayout
import Qtc.Gui.QWidget

type MyQDialog = QWidgetSc (CMyQDialog)
data CMyQDialog = CMyQDialog

myQDialog :: IO (MyQDialog)
myQDialog = qSubClass $ qWidget ()

main :: IO ()
main = do
  -- Need to construct a Qapplication before painting
  _app <- qApplication ()
  dialog <- myQDialog
  -- mb <- qMessageBox dialog
  mainLayout <- qVBoxLayout ()
  grid       <- add_grid
  addWidget mainLayout grid
  setLayout dialog mainLayout
  setWindowTitle dialog "Basic Reactivity"
  qshow dialog ()
  _ok <- qApplicationExec ()
  return()

add_grid :: IO (QWidget a)
add_grid = do
  gridGroupBox <- qGroupBox "Demo"
  gridLayout <- qGridLayout ()

  -- Add Text boxes
  l1  <- reactiveQLineEdit
  addWidget gridLayout (l1, 0 :: Int, 1::Int)

  l2  <- reactiveQLineEdit
  addWidget gridLayout (l2, 1 :: Int, 1::Int)

  -- Add Button
  tb <- reactiveQPushButton "Copy Up"
  addWidget gridLayout (tb, 1 :: Int, 2::Int)

  -- Make widgets reactive
  e1  <- lineEditText l1
  e2  <- lineEditText l2
  btn <- buttonClick tb

  -- Connection: rule
  e1 <:= (btn `governingR` e2 :: ReactiveFieldRead IO String)
  e1 =:= (involution reverse <$$> e2)

  setColumnStretch gridLayout (2, 20)
  setLayout gridGroupBox gridLayout
  return $ objectCast gridGroupBox

-- | Temporary: will be moved to Keera Hails' Reactive Values library.
governingR :: (ReactiveValueRead a b m,  ReactiveValueRead c d m)
           => a -> c -> ReactiveFieldRead m d
governingR r c = ReactiveFieldRead getter notifier
  where getter   = reactiveValueRead c
        notifier = reactiveValueOnCanRead r
