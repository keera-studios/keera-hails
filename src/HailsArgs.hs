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
                             ]
             &= help "Hails action to execute (init)"
             &= typ "ACTION"
             }
         &= summary "Hails"
         &= program "hails"

