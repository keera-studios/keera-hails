module Control.Exception.Extra where

import qualified Control.Exception as E

-- | Returns a given computation ignoring an exception
anyway :: a -> E.SomeException -> a
anyway f _ = f

-- | Tries to execute all the IO computations
-- until one succeeds
trySeq :: [IO ()] -> IO ()
trySeq []     = return ()
trySeq (x:xs) = E.handle (anyway (trySeq xs)) x
