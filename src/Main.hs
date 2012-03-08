{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

-- External
import Data.Maybe
import System.Console.CmdArgs
import System.Directory
import System.FilePath

-- Internal
import AppDataBasic
import AppDataFull
import HailsArgs
import Paths_hails

main :: IO ()
main = do
  appState <- cmdArgs sample
  
  -- This point will only be reached when appropriate
  
  -- Try to build a complete action with those values that can be
  -- guessed from existing files.
  act <- buildHailsAction appState
  
  -- Execute that action.
  executeHailsAction act

buildHailsAction :: AppDataBasic -> IO AppDataFull          
buildHailsAction (AppDataBasic { action, outputDir }) =
   return $ AppDataFull { action  = action, outputDir = dir' }
 where dir' = fromMaybe "." outputDir

-- This is the action execution
executeHailsAction :: AppDataFull -> IO ()
executeHailsAction (AppDataFull { action, outputDir }) = copyTemplates outputDir

-- Copy all the templates
copyTemplates :: FilePath -> IO ()
copyTemplates dir = mapM_ (copyTemplate dir) templates
       
-- Copy one template
copyTemplate :: FilePath -> FilePath -> IO()
copyTemplate dir fp = do
  fullpath <- getDataFileName $ "templates" </> fp
  copyFile fullpath (dir </> fp)

-- Standard template list
templates :: [ FilePath ]
templates = [ "Main.hs"
            , "CombinedEnvironment.hs"
            , "Model/ProtectedModel.hs"
            , "Model/ProtectedModel/ProtectedModelInternals.hs"
            , "Model/ReactiveModel.hs"
            , "Model/ReactiveModel/ReactiveModelInternals.hs"
            , "Controller.hs"
            , "View.hs"
            ]
