module Hails.MVC.Controller.Conditions.Config where

import qualified Control.Exception as E
import           Control.Monad
import           System.FilePath
import           System.Directory

import Control.Exception.Extra

-- | A config IO layer reads and writes 
-- an environment from a string. It's like a
-- read/show combination for configuration files
-- to and from Environments
type ConfigIO e = ( String -> e -> IO () -- Writer
                  , e -> IO String       -- Reader
                  )

defaultRead :: ConfigIO e -> String -> e -> IO()
defaultRead (readConf, _) app cenv = 
  void $ E.handle (anyway (return ())) $ do
    dir <- getAppUserDataDirectory app
    let file = dir </> "config"
    c <- readFile file
    readConf c cenv

defaultWrite :: ConfigIO e -> String -> e -> IO()
defaultWrite (_, showConf) app cenv = 
  void $ E.handle (anyway (return ())) $ do
    dir <- getAppUserDataDirectory app
    createDirectoryIfMissing True dir
    let file = dir </> "config"
    writeFile file =<< showConf cenv