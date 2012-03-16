module System.Application where

-- External imports
import Control.Monad
-- import Control.Monad.Extra
import System.GIO.File.AppInfo
import System.Process

-- FIXME: This uses runProcess instead of appInfoLaunchUris because
-- the latter segfaults in my machine
openUrlBySystemTool :: String -> IO ()
openUrlBySystemTool url = do
  infos <- appInfoGetAllForType "text/html"
  unless (null infos) $ void $ do
    let exe = appInfoGetExecutable $ head infos
    runProcess exe [url] Nothing Nothing Nothing Nothing Nothing
