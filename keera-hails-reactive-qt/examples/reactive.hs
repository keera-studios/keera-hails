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

type MyQPushButton = QPushButtonSc (CMyQPushButton)
data CMyQPushButton = CMyQPushButton

myQPushButton :: String -> IO (MyQPushButton)
myQPushButton t = qSubClass $ qPushButton t

main :: IO ()
main
  = do
    app <- qApplication ()
    dialog <- myQDialog
    mb <- qMessageBox dialog
    mainLayout <- qVBoxLayout ()
    setMenuBar mainLayout =<< add_menuBar dialog
    let wfl = [(add_horizontal mb dialog),
               (add_grid),
               (add_bigEditor),
               (add_buttonBox dialog)]
    mapM_ (m1 mainLayout) wfl
    setLayout dialog mainLayout
    setWindowTitle dialog "Basic Layouts"
    qshow dialog ()
    ok <- qApplicationExec ()
    return()

m1 :: QVBoxLayout () -> (IO (QWidget a)) -> IO ()
m1 mainLayout wf
  = do
    w <- wf
    addWidget mainLayout w

add_menuBar :: MyQDialog -> IO (QMenuBar ())
add_menuBar dlg
  = do
    menuBar <- qMenuBar ()
    fileMenu <- qMenu ("&File", dlg)
    exitAction <- addAction fileMenu "E&xit"
    addMenu menuBar fileMenu 
    app <- qCoreApplicationInstance ()
    connectSlot exitAction "triggered()" app "quit()" ()
    return menuBar

add_horizontal :: QMessageBox () -> MyQDialog -> IO (QWidget a)
add_horizontal mb dlg
  = do
    horizontalGroupBox <- qGroupBox "Horizontal layout"
    horizontalLayout <- qHBoxLayout ()
    add_buttons mb dlg horizontalLayout "Button" 4 0
    setLayout horizontalGroupBox horizontalLayout
    return $ objectCast horizontalGroupBox

add_grid :: IO (QWidget a)
add_grid = do
  gridGroupBox <- qGroupBox "Grid layout"
  gridLayout <- qGridLayout ()
  -- add_lablins gridLayout "Lines" 3 0
  (l1, l2) <- add_lablins gridLayout "Lines"
  tb <- myQPushButton "hi"
  addWidget gridLayout (tb, 1 :: Int, 3::Int)

  -- Make button clicks transfer text onto the text entry
  e1  <- lineEditText l1
  e2  <- lineEditText l2
  btn <- buttonClick tb

  -- Combine btn and e2
  let f :: ReactiveFieldRead IO String
      f = liftR2 e2 btn (\t _ -> t)

  -- Connection: rule
  e1 <:= f

  smallEditor <- qTextEdit ()
  setPlainText smallEditor "This widget takes up about two thirds of the grid layout."
  addWidget gridLayout (smallEditor, 0::Int, 2::Int, 4::Int, 1::Int)

add_bigEditor :: IO (QWidget a)
add_bigEditor
  = do
    bigEditor <- qTextEdit ()
    setPlainText bigEditor "This widget takes up all the remaining space in the top-level layout."
    return $ objectCast bigEditor

add_buttonBox :: MyQDialog -> IO (QWidget a)
add_buttonBox dlg
  = do
    buttonBox <- qDialogButtonBox ((fOk + fCancel)::QDialogButtonBoxStandardButtons)
    connectSlot buttonBox "accepted()" dlg "close()" ()
    connectSlot buttonBox "rejected()" dlg "close()" ()
    return $ objectCast buttonBox

add_buttons :: QMessageBox () -> MyQDialog -> QHBoxLayout () -> String -> Int -> Int -> IO ()
add_buttons _ _ _ _ nb cn | cn >= nb = return ()
add_buttons mb dlg qlt bs nb cn
  = do
    let cc = cn + 1
    tb <- myQPushButton $ bs ++ show cc
    connectSlot tb "clicked()" tb "click()" $ on_button_clicked mb dlg
    addWidget qlt tb
    add_buttons mb dlg qlt bs nb cc

add_lablins :: QGridLayout () -> String -> Int -> Int -> IO ()
add_lablins _ _ nl cn | cn >= nl = return () 
add_lablins qlt ls nl cn
  = do
    let cc = cn + 1
    nlb <- qLabel $ ls ++ show cc
    nln <- qLineEdit ()
    addWidget qlt (nlb, cc, 0::Int)
    addWidget qlt (nln, cc, 1::Int)
    add_lablins qlt ls nl cc

on_button_clicked :: QMessageBox () -> MyQDialog -> MyQPushButton -> IO ()
on_button_clicked mb dlg this
  = do
    tt <- text this ()
    setText mb $ "You have clicked " ++ show tt
    qshow mb ()
    returnGC

