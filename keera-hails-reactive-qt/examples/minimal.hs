{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
{-| Program   : widgets.hs
    Copyright : (c) David Harley 2010
    Project   : qtHaskell
    Version   : 1.1.4
    Modified  : 2010-09-02 17:02:47
    
    Warning   : this file is machine generated - do not modify.
--}
-----------------------------------------------------------------------------

module Main where

import Graphics.UI.Qt.Reactive
import Control.Applicative
import Control.Concurrent
import Control.GFunctor
import Data.ReactiveValue
import Qtc.ClassTypes.Core
import Qtc.ClassTypes.Gui
import Qtc.Classes.Base
import Qtc.Classes.Qccs
import Qtc.Classes.Qccs_h
import Qtc.Classes.Core
import Qth.ClassTypes.Core
import Qth.Core.Size
import Qth.Core.Rect
import Qtc.Core.QSize
import Qtc.Classes.Gui
import Qtc.Enums.Base
import Qtc.Enums.Classes.Core
import Qtc.Core.Base
import Qtc.Gui.Base
import Qtc.Core.QCoreApplication
import Qtc.Gui.QApplication
import Qtc.Gui.QDialog
import Qtc.Gui.QMenuBar
import Qtc.Gui.QMenu
import Qtc.Gui.QLayout
import Qtc.Gui.QGroupBox
import Qtc.Gui.QDialogButtonBox
import Qtc.Enums.Gui.QDialogButtonBox
import Qtc.Gui.QVBoxLayout
import Qtc.Gui.QHBoxLayout
import Qtc.Gui.QGridLayout
import Qtc.Gui.QTextEdit
import Qtc.Gui.QLineEdit
import Qtc.Gui.QLabel
import Qtc.Gui.QWidget
import Qtc.Gui.QPushButton
import Qtc.Gui.QPushButton_h
import Qtc.Gui.QAbstractButton
import Qtc.Gui.QMessageBox
import Qtc.Enums.Core.Qt

type MyQDialog = QWidgetSc (CMyQDialog)
data CMyQDialog = CMyQDialog

myQDialog :: IO (MyQDialog)
myQDialog = qSubClass $ qWidget ()

type MyQPushButton = QPushButtonSc CMyQPushButton
data CMyQPushButton = CMyQPushButton

myQPushButton :: String -> IO (MyQPushButton)
myQPushButton t = qSubClass $ qPushButton t

-- type MyQLineEdit = QLineEditSc CMyQLineEdit
-- data CMyQLineEdit = CMyQLineEdit
--
-- myQLineEdit :: IO MyQLineEdit
-- myQLineEdit = qSubClass $ qLineEdit ()

main :: IO ()
main = do
  app <- qApplication ()
  dialog <- myQDialog
  mb <- qMessageBox dialog
  mainLayout <- qVBoxLayout ()
  grid       <- add_grid
  addWidget mainLayout grid
  setLayout dialog mainLayout
  setWindowTitle dialog "Basic Reactivity"
  qshow dialog ()
  ok <- qApplicationExec ()
  return()

add_grid :: IO (QWidget a)
add_grid = do
  gridGroupBox <- qGroupBox "Demo"
  gridLayout <- qGridLayout ()

  -- Add Text boxes
  l1  <- reactiveQLineEdit
  -- connectSlot l1 "textChanged(QString)" l1 "onTextChanged(QString)" (\(_ :: QObject a) (_ :: String) -> print "p")
  addWidget gridLayout (l1, 0 :: Int, 1::Int)

  l2  <- reactiveQLineEdit
  addWidget gridLayout (l2, 1 :: Int, 1::Int)

  -- Add Button
  -- tb <- myQPushButton "Copy Up"
  tb <- myQPushButton "Copy Up"
  addWidget gridLayout (tb, 1 :: Int, 2::Int)

  -- Make button clicks transfer text onto the text entry
  e1  <- lineEditText l1
  e2  <- lineEditText l2
  btn <- buttonClick tb

  -- Connection: rule
  e1 <:= (btn `governingR` e2 :: ReactiveFieldRead IO String)
  e1 =:= (involution reverse <$$> e2)

  setColumnStretch gridLayout (2, 20)
  setLayout gridGroupBox gridLayout
  return $ objectCast gridGroupBox

governingR :: (ReactiveValueRead a b m,  ReactiveValueRead c d m)
           => a -> c -> ReactiveFieldRead m d
governingR r c = ReactiveFieldRead getter notifier
  where getter   = reactiveValueRead c
        notifier = reactiveValueOnCanRead r
