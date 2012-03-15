{-# LANGUAGE CPP #-}
-- | This module contains the function we need to use to get automatic
-- translation on all the strings in our programs.
module Hails.I18N.Gettext where

import Text.I18N.GetText
import System.IO.Unsafe
import Codec.Binary.UTF8.String
 
__ :: String -> String
__ s 
#ifdef linux_HOST_OS
 | isUTF8Encoded s' = decodeString s'
#endif
 | otherwise        = s'
  where s' = unsafePerformIO $ getText s
