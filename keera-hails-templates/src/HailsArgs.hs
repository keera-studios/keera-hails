module HailsArgs where

-- External
import System.Console.CmdArgs

-- Internal
import AppDataBasic

-- This is the cmdArgs-based CLI interface definition
sample :: AppDataBasic
sample = AppDataBasic
             { action = enum [ HailsInit 
                             &= explicit 
                             &= name "init" 
                             &= help "(Re-)start a project" 
                             , HailsClean
                             &= explicit
                             &= name "clean"
                             &= help "Delete unmodified templates"
                             ]
             &= help "Hails action to execute (init)"
             &= typ "ACTION"
             , outputDir = def
             &= explicit
             &= name "output-dir"
             &= help "Directory where generated files will be placed"
             &= typ "DIR"
             , overwrite = def
             &= help "Overwrite existing files"
             }
         &= summary "Hails"
         &= program "hails"

