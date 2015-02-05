{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module System.Environment.SetEnv (setEnv) where

#ifdef linux_HOST_OS
import           Foreign.C.Error  ( throwErrnoIfMinus1_ )
import           Foreign.C.String
import           Foreign.C.Types  ( CInt(..) )
import           System.Posix.Internals

{- |The 'setEnv' function inserts or resets the environment variable name in
    the current environment list.  If the variable @name@ does not exist in the
    list, it is inserted with the given value.  If the variable does exist,
    the argument @overwrite@ is tested; if @overwrite@ is @False@, the variable is
    not reset, otherwise it is reset to the given value.
 -}

setEnv :: String -> String -> IO ()
setEnv key value =
  withFilePath key $ \ keyP ->
    withFilePath value $ \ valueP ->
      throwErrnoIfMinus1_ "setenv" $
        c_setenv keyP valueP (fromIntegral (fromEnum True))

foreign import ccall unsafe "setenv"
   c_setenv :: CString -> CString -> CInt -> IO CInt
#else

import Foreign.C.String

foreign import ccall unsafe "putenv"
  c_putenv :: CString -> IO ()

setEnv :: String -> String -> IO()
setEnv key value = 
 withCString (key ++ "=" ++ value) $ \c_pair ->
 c_putenv c_pair

-- foreign import stdcall unsafe "windows.h SetEnvironmentVariableW"
--  c_SetEnvironmentVariable :: LPCTSTR -> LPCTSTR -> IO Bool
-- 
-- setEnvironmentVariable :: String -> String -> IO Bool
-- setEnvironmentVariable key value = 
--  withTString key $ \c_key ->
--  withTString value $ \c_value ->
--  c_SetEnvironmentVariable c_key c_value
#endif
