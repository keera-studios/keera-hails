{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.UI.Qt.Reactive
   ( buttonClick
   , lineEditText
   , ReactQLineEdit
   , reactiveQLineEdit
   , ReactQPushButton
   , reactiveQPushButton
   )
  where

import Control.Concurrent
import Data.ReactiveValue

import Qtc.ClassTypes.Core
import Qtc.ClassTypes.Gui
import Qtc.Classes.Base
import Qtc.Classes.Qccs
import Qtc.Core.Base
import Qtc.Gui.QLineEdit
import Qtc.Gui.QPushButton

-- * Line edits

type ReactQLineEdit = QLineEditSc CReactQLineEdit
data CReactQLineEdit = CReactQLineEdit 
 
reactiveQLineEdit :: IO ReactQLineEdit 
reactiveQLineEdit = qSubClass $ qLineEdit () 

-- | Make a line edit's text reactive.
lineEditText :: ReactQLineEdit -> IO (ReactiveFieldReadWrite IO String)
lineEditText entry = do
  notifiers <- notifiersNew

  connectSlot entry "textChanged(QString)" entry "onTextChanged(QString)"
      (\(_ :: QObject a) (_ :: String) -> runNotifiers notifiers)

  let getter   = text entry ()
      setter v = do -- Break circularity
                    t <- text entry ()
                    when (t /= v) $ do setText entry v
                                       runNotifiers notifiers
      notifier = addNotifier notifiers

  return $ ReactiveFieldReadWrite setter getter notifier

-- * Buttons

type ReactQPushButton  = QPushButtonSc CReactQPushButton
data CReactQPushButton = CReactQPushButton

reactiveQPushButton :: String -> IO ReactQPushButton
reactiveQPushButton t = qSubClass $ qPushButton t

-- | Make a button click reactive.
buttonClick :: QPushButtonSc a -> IO (ReactiveFieldRead IO ())
buttonClick tb = do
  notifiers <- notifiersNew

  -- Connect to click signal
  connectSlot tb "clicked()" tb "click()"
    (\(_ :: QObject a) -> runNotifiers notifiers)

  let getter   = return ()
      notifier = addNotifier notifiers

  return $ ReactiveFieldRead getter notifier

-- * Internal (notifier queue)

-- | This is auxiliary code for a list of notifiers (event handlers).
--   It will eventually be moved to a separate module.
type Notifiers = MVar [IO ()]

notifiersNew :: IO Notifiers
notifiersNew = newMVar []

runNotifiers :: Notifiers -> IO()
runNotifiers ntfs = readMVar ntfs >>= sequence_

addNotifier :: Notifiers -> IO () -> IO()
addNotifier ntfs p = modifyMVar_ ntfs (\x -> return (x ++ [p]))
