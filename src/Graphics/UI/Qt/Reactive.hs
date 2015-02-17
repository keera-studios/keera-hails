{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.UI.Qt.Reactive
   ( buttonClick
   , lineEditText
   )
  where

import Control.Concurrent
import Data.ReactiveValue
import Qtc.ClassTypes.Core
import Qtc.ClassTypes.Gui
import Qtc.Classes.Base
import Qtc.Classes.Qccs
import Qtc.Core.Base
import Qtc.Gui.QLineEdit ()

-- | Make a line edit's text reactive. Does not fully work yet
--   read/write work but the changes are not detected.
lineEditText :: QLineEdit a -> IO (ReactiveFieldReadWrite IO String)
lineEditText entry = do
  notifiers <- notifiersNew

  connectSlot entry "textChanged(string)" entry "onTextChanged(string)"
      (\(_ :: QObject a) s -> print $ s ++ "p")

  let getter   = text entry ()
      setter v = do t <- text entry ()
                    when (t /= v) $ do setText entry v
                                       runNotifiers notifiers
      notifier = addNotifier notifiers

  return $ ReactiveFieldReadWrite setter getter notifier

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

-- | This is auxiliary code for a list of notifiers (event handlers).
--   It will eventually be moved to a separate module.
type Notifiers = MVar [IO ()]

notifiersNew :: IO Notifiers
notifiersNew = newMVar []

runNotifiers :: Notifiers -> IO()
runNotifiers ntfs = readMVar ntfs >>= sequence_

addNotifier :: Notifiers -> IO () -> IO()
addNotifier ntfs p = modifyMVar_ ntfs (\x -> return (x ++ [p]))
