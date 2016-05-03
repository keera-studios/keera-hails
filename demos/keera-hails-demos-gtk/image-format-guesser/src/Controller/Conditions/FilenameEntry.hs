module Controller.Conditions.FilenameEntry where

-- Internal libraries
import CombinedEnvironment
import View
import View.MainWindow.Objects
import Model.ProtectedModel
import qualified Extra.UI.Simplify.EntryBasic as Extra

installHandlers :: CRef -> IO()
installHandlers =
  Extra.installHandlers
    [ FilenameChanged ]
    (mainWindowFilenameEntry . mainWindowBuilder)
    -- Setter
    setFilename
    -- Getter
    ((fmap Just) . getFilename)
