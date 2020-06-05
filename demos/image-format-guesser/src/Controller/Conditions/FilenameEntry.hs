-- |
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Controller.Conditions.FilenameEntry where

-- Internal libraries
import CombinedEnvironment
import View
import View.MainWindow.Objects
import Model.ProtectedModel
import qualified Extra.UI.Simplify.EntryBasic as Extra

installHandlers :: CEnv -> IO()
installHandlers =
  Extra.installHandlers
    [ FilenameChanged ]
    (mainWindowFilenameEntry . mainWindowBuilder)
    -- Setter
    setFilename
    -- Getter
    ((fmap Just) . getFilename)
