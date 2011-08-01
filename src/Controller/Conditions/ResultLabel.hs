module Controller.Conditions.ResultLabel where

-- Internal libraries
import CombinedEnvironment
import View
import View.MainWindow.Objects
import Model.ProtectedModel
import qualified Extra.UI.Simplify.LabelBasic as Extra

installHandlers :: CRef -> IO()
installHandlers =
  Extra.installHandlers
    [ FilenameChanged ]
    (mainWindowMessageLbl . mainWindowBuilder)
    -- Getter
    getFilename
