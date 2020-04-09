-- | Install/load the right Gettext files for your chosen language and
--   application.
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Hails.I18N.Language where

import qualified Control.Exception         as E
import           Control.Exception.Extra   (anyway)
import           Control.Monad             (unless, void)
import           Data.Maybe                (fromMaybe, listToMaybe)
import           System.Directory          (getAppUserDataDirectory)
import           System.Environment.SetEnv (setEnv)
import           System.FilePath           ((</>))
import           System.Locale.SetLocale   (Category (LC_ALL), setLocale)
import           Text.I18N.GetText         (bindTextDomain, textDomain)

-- | Install the current language using the LC_ALL and LANGUAGE
-- environment variables and other gettext methods. This requires
-- the application's name, and it loads the language from a file
-- called "default-language" in the application's config dir.
installLanguage :: String -> IO ()
installLanguage appName = void $ do

    -- Read the config file if it exists
    userDataDir <- getAppUserDataDirectory appName
    let languageFile = userDataDir </> "default-language"

    -- lang == "" if no value was found
    lang <- E.handle (anyway (return "")) $ do
              (safeHead "" . lines) <$> readFile languageFile

    -- Update locale and language only if a value has been found
    unless (null lang) $ E.handle (anyway (return ())) $ do
      setLocale LC_ALL (Just lang)
      setEnv "LANGUAGE" lang
    bindTextDomain appName $ Just "."
    textDomain $ Just appName

  where

    safeHead :: a -> [a] -> a
    safeHead x = fromMaybe x . listToMaybe
