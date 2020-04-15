{-# LANGUAGE CPP #-}
-- | This module contains the function we need to use to get automatic
-- translation on all the strings in our programs.
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Hails.I18N.Gettext where

import Codec.Binary.UTF8.String (decodeString, isUTF8Encoded)
import System.IO.Unsafe         (unsafePerformIO)
import Text.I18N.GetText        (getText)

-- | Translate a string using gettext.
--
--   Note: This implementation decodes UTF-8 strings only in Linux. If it
--   should also in other OSs, please open an issue on github.
__ :: String -> String
__ string
#ifdef linux_HOST_OS
    | isUTF8Encoded translation = decodeString translation
#endif
    | otherwise                 = translation
  where
    translation :: String
    translation = unsafePerformIO $ getText string
