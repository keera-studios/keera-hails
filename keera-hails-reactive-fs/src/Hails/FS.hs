{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
-- |
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Hails.FS where

import Prelude hiding (FilePath)
import Control.Concurrent.MVar
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever)
import Data.ReactiveValue
import Filesystem.Path.CurrentOS
import System.Directory
import System.FSNotify

-- | A file as a passive reactive value.
--
-- Passive values are those that never notify of changes to them. They are
-- useful as sources of information controlled by other RVs (buttons, etc.)
pasiveFileReactive :: FilePath -> ReactiveFieldReadWrite IO String
pasiveFileReactive fp = ReactiveFieldReadWrite setter getter notifier
 where getter     = readFile  (encodeString fp)
       setter v   = writeFile (encodeString fp) v
       notifier _ = return ()

-- | A file as a reactive value. The file must exist at the time
-- the call is evaluated.

-- TODO: Make it ok for the file not to exist.
-- TODO: Capture and ignore exceptions in readFile and writeFile.
fileReactive :: FilePath -> IO (ReactiveFieldReadWrite IO String)
fileReactive fp = do
  fpP <- canonicalizePath (encodeString fp)
  notifiers <- newMVar []
  let getter     = readFile  (encodeString fp)   -- fails if the path does not exist
      setter v   = writeFile (encodeString fp) v -- may fail
      notify     = sequence_ =<< readMVar notifiers
      notifier p = modifyMVar_ notifiers (\x -> return (x ++ [p]))

  -- Run the notification manager, ignore result (thread)
  forkIO $ withManager $ \mgr -> do
    _ <- watchDir mgr                            -- manager
                  (encodeString $ directory fp)  -- directory to watch
                  (\e -> eventPath e == fpP)     -- predicate
                  (const notify)                 -- notifier
    forever $ threadDelay maxBound
  return $ ReactiveFieldReadWrite setter getter notifier
