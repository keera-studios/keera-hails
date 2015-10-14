module Graphics.UI.WX.Reactive where

import Control.Concurrent
import Data.ReactiveValue
import Graphics.UI.WX as WX
import Graphics.UI.WXCore as WXCore

buttonClick :: Button () -> IO (ReactiveFieldRead IO ())
buttonClick btn = do
  notifiers <- newMVar []
  set btn [ on command := readMVar notifiers >>= sequence_ ]
  let getter = return ()
      notifier p = modifyMVar_ notifiers (\x -> return (x ++ [p]))
  return $ ReactiveFieldRead getter notifier

entryText :: TextCtrl () -> IO (ReactiveFieldReadWrite IO String)
entryText entry = do
  notifiers <- notifiersNew

  set entry [ on onText :~ \kbd -> kbd >> runNotifiers notifiers ]

  let getter   = get entry text
      setter v = do t <- get entry text
                    when (t /= v) $ do set entry [ text := v ]
                                       runNotifiers notifiers
      notifier = addNotifier notifiers
  return $ ReactiveFieldReadWrite setter getter notifier

-- Literally taken from Reactive Banana
onText :: WX.Event (WXCore.Control a) (IO ())
onText = WX.newEvent "onText" WXCore.controlGetOnText WXCore.controlOnText

type Notifiers = MVar [IO ()]

notifiersNew :: IO Notifiers
notifiersNew = newMVar []

runNotifiers :: Notifiers -> IO()
runNotifiers ntfs = readMVar ntfs >>= sequence_

addNotifier :: Notifiers -> IO () -> IO()
addNotifier ntfs p = modifyMVar_ ntfs (\x -> return (x ++ [p]))
