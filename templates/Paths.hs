{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Paths (getDataFileName) where

-- These imports are necessary only in Windows
#ifndef linux_HOST_OS
import System.Win32.Types
import System.Win32.Registry

import System.Environment
import Foreign.Ptr (castPtr)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.C.String (peekCWString, withCWString)
import Control.Exception (bracket, throwIO, handle)
import Control.Exception.Extra (anyway)
#endif

import qualified Paths.CustomPaths as P

-- This code is only used in Windows. It depends on the Windows registry
#ifndef linux_HOST_OS
-- // parse a string from a registry value of certain type
parseRegString :: RegValueType -> LPBYTE -> IO String
parseRegString ty mem
   | ty == rEG_SZ        = peekCWString (castPtr mem)
   | ty == rEG_EXPAND_SZ = peekCWString (castPtr mem) >>=
                              expandEnvironmentStrings
   | otherwise           = ioError (userError "Invalid registry value type")

-- // FFI import of the ExpandEnvironmentStrings function needed
-- // to make use of the registry values
expandEnvironmentStrings :: String -> IO String
expandEnvironmentStrings toexpand =
   withCWString toexpand $ \input ->
   allocaBytes 512 $ \output ->
   do c_ExpandEnvironmentStrings input output 256
      peekCWString output
foreign import stdcall unsafe "windows.h ExpandEnvironmentStringsW"
  c_ExpandEnvironmentStrings :: LPCTSTR -> LPTSTR -> DWORD -> IO DWORD

getAppKey :: IO HKEY
getAppKey = 
  handle
   (anyway (openKey hKEY_LOCAL_MACHINE regPath))
   (openKey hKEY_CURRENT_USER regPath)

-- The default program's key in the registry
regPath :: String
regPath = "SOFTWARE\\" ++ vendorKey ++ "\\" ++ programKey

openKey :: HKEY -> String -> IO HKEY
openKey k path = regOpenKeyEx k path kEY_QUERY_VALUE

-- Search for a value labeled 'Path' in the application HKey
getAppPathFromReg :: IO String
getAppPathFromReg =
   bracket getAppKey regCloseKey $ \usfkey ->
   allocaBytes 512 $ \mem ->
   do ty <- regQueryValueEx usfkey "Path" mem 512
      parseRegString ty mem

#endif

-- This part is OS-dependent
getDataDir :: IO FilePath
getDataDir =  
#ifndef linux_HOST_OS
  handle (anyway P.getDataDir) $ do
    fp <- getAppPathFromReg
    return (fp ++ pathSeparator:"data")
#else
   do fp <- P.getDataDir
      return (fp ++ pathSeparator:"data")
#endif

getDataFileName :: FilePath -> IO FilePath
getDataFileName name =
  fmap (`joinFileName` name) getDataDir
  -- myPutStrLn $ "Going to open " ++ res
  -- return res

joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (last dir) = dir ++ fname
  | otherwise                  = dir ++ pathSeparator:fname

pathSeparator :: Char
pathSeparator =
#ifndef linux_HOST_OS
  '\\'
#else
  '/'
#endif

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
