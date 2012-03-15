module Control.Exception.Extra where

import GHC.Conc
import System.Glib.GError
import qualified Control.Exception as E

-- | Returns a given computation ignoring an exception
anyway :: a -> E.SomeException -> a
anyway f _ = f

-- | Tries to execute all the IO computations
-- until one succeeds
trySeq :: [IO ()] -> IO ()
trySeq []     = return ()
trySeq (x:xs) = E.handle (anyway (trySeq xs)) x

-- | Handles any exception (apparently the default handle won't handle all)
handleAllExceptions :: IO () -> IO () -> IO ()
handleAllExceptions handler op = do
  setUncaughtExceptionHandler (anyway handler)
  E.handle (anyway handler) $ handleGError (anywayG handler) op

anywayG :: IO a -> GError -> IO a
anywayG x (GError _dom _code _msg) = x
