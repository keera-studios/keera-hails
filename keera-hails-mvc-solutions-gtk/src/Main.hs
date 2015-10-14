{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

-- External
import Control.Monad
import Control.Monad.IfElse
import Data.Maybe
import System.Console.CmdArgs
import System.Directory
import System.FilePath
import qualified Data.ByteString.Lazy as L

-- Internal
import AppDataBasic
import AppDataFull  as F
import HailsArgs
import Paths_hails

main :: IO ()
main = do
  appState <- cmdArgs sample
  
  -- This point will only be reached when appropriate
  
  -- Try to build a complete action with those values that can be
  -- guessed from existing files.
  conf <- buildHailsConf appState
  
  -- Execute that action.
  executeHailsAction conf

buildHailsConf :: AppDataBasic -> IO AppDataFull          
buildHailsConf (AppDataBasic { action, outputDir, overwrite }) =
   return $ AppDataFull { action    = action
                        , outputDir = dir'
                        , overwrite = overwrite
                        }
 where dir' = fromMaybe "." outputDir

-- This is the action execution
executeHailsAction :: AppDataFull -> IO ()
executeHailsAction app =
  case F.action app of
   HailsInit  -> copyTemplates app
   HailsClean -> cleanTemplates app

-- Clean unmodified templates
cleanTemplates :: AppDataFull -> IO ()
cleanTemplates conf = mapM_ (cleanTemplate conf) templates

cleanTemplate :: AppDataFull -> FilePath -> IO()
cleanTemplate conf fp = do
  fullpath <- getDataFileName $ "templates" </> fp
  let dir    = F.outputDir conf
      dest   = (dir </> fp)
  exists <- doesFileExist dest
  when exists $ whenM (fileEq fullpath dest) $ 
     removeFile dest

fileEq :: FilePath -> FilePath -> IO Bool
fileEq fp1 fp2 = do
  c1 <- L.readFile fp1
  c2 <- L.readFile fp2
  return (c1 == c2)

-- Copy all the templates
copyTemplates :: AppDataFull -> IO ()
copyTemplates conf = mapM_ (copyTemplate conf) templates
       
-- Copy one template
copyTemplate :: AppDataFull -> FilePath -> IO()
copyTemplate conf fp = do
  fullpath <- getDataFileName $ "templates" </> fp
  let dir    = F.outputDir conf
      dest   = (dir </> fp)
      parent = takeDirectory dest
      overw  = F.overwrite conf
  exists <- doesFileExist dest
  when (not exists || overw) $ do
    createDirectoryIfMissing True parent
    copyFile fullpath dest

-- Standard template list
templates :: [ FilePath ]
templates = [ "Main.hs"
            , "CombinedEnvironment.hs"
            , "Model/Model.hs"
            , "Model/ProtectedModel.hs"
            , "Model/ProtectedModel/ProtectedFields.hs"
            , "Model/ProtectedModel/ProtectedModelInternals.hs"
            , "Model/ReactiveModel.hs"
            , "Model/ReactiveModel/ModelEvents.hs"
            , "Model/ReactiveModel/ReactiveFields.hs"
            , "Model/ReactiveModel/ReactiveModelInternals.hs"
            , "Controller.hs"
            , "Controller/Conditions.hs"
            , "Paths.hs"
            , "Paths/CustomPaths.hs"
            , "View.hs"
            , "View/Objects.hs"
            ]
