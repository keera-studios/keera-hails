module Hails.Graphics.UI.Gtk.Helpers.FileDialog
  ( openOpenFileDialog
  )
  where

import Graphics.UI.Gtk

type Ext = ([String], String)

-- Auxiliary functions: open file dialogs
openOpenFileDialog :: String -> [Ext] -> IO (Maybe String)
openOpenFileDialog title exts = do

  dialog <- fileChooserDialogNew
              (Just title)           --dialog title
              Nothing
              FileChooserActionOpen  --the kind of dialog we want
              [("gtk-cancel"         --The buttons to display
               ,ResponseCancel)
              ,("gtk-ok"
               , ResponseAccept)]

  ffs <- extsToFilters exts
  mapM_ (fileChooserAddFilter dialog) ffs

  widgetShow dialog
  result <- dialogRun dialog
  res <- case result of
           ResponseAccept -> fileChooserGetFilename dialog
           _              -> return Nothing
  widgetDestroy dialog
  return res

extsToFilters :: [Ext] -> IO [FileFilter]
extsToFilters = mapM extToFilter

extToFilter :: Ext -> IO FileFilter
extToFilter (pats, name) = do
  ff <- fileFilterNew
  fileFilterSetName ff name
  mapM_ (fileFilterAddPattern ff) pats
  return ff
