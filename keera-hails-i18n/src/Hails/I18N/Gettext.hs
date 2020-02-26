{-# LANGUAGE CPP #-}
-- | This module contains the function we need to use to get automatic
-- translation on all the strings in our programs.
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Hails.I18N.Gettext where

import Text.I18N.GetText
import System.IO.Unsafe
import Codec.Binary.UTF8.String

-- | Translate a string using gettext.
--
--   Note: This implementation decodes UTF-8 strings only in Linux. If it
--   should also in other OSs, please open an issue on github.
__ :: String -> String
__ s
#ifdef linux_HOST_OS
 | isUTF8Encoded s' = decodeString s'
#endif
 | otherwise        = s'
  where s' = unsafePerformIO $ getText s
