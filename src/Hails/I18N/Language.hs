module Hails.I18N.Language where

import qualified Control.Exception as E
import           Control.Exception.Extra
import           Control.Monad
import           System.Directory
import           System.FilePath
import           System.Locale.SetLocale
import           System.Environment.SetEnv
import           Text.I18N.GetText

-- | Installs the current language using the LC_ALL and LANGUAGE
-- environment variables and other gettext methods.
installLanguage :: String -> IO ()
installLanguage app = void $ do

  -- Read the config file if it exists
  dir <- getAppUserDataDirectory app
  let file = dir </> "default-language"

  -- lang == "" if no value was found
  lang <- E.handle (anyway (return "")) $ do
             cs <- fmap lines $ readFile file
             return $ if null cs then "" else head cs
  
  -- Update locale and language only if a value has been found
  unless (null lang) $ E.handle (anyway (return ())) $ do
    setLocale LC_ALL (Just lang)
    setEnv "LANGUAGE" lang
  bindTextDomain app $ Just "." 
  textDomain $ Just app
