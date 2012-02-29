module Hails.Graphics.UI.Gtk.Helpers.Combo where

import Data.List
import Data.Maybe
import Graphics.UI.Gtk

addTextColumn :: (TreeModelClass (model row), TypedTreeModelClass model)
                => ComboBox -> model row -> (row -> Maybe String) -> IO()
addTextColumn cb st f = do

  comboBoxSetModel cb (Just st)
  renderer <- cellRendererTextNew
  cellLayoutPackStart cb renderer True
  cellLayoutSetAttributes cb renderer st $ map (cellText :=).maybeToList.f
  return ()

type TypedComboBox a = (ComboBox, ListStore a)

typedComboBoxCombo :: TypedComboBox a -> ComboBox
typedComboBoxCombo = fst

typedComboBoxStore :: TypedComboBox a -> ListStore a
typedComboBoxStore = snd

typedComboBoxGetSelected :: TypedComboBox a -> IO (Maybe a)
typedComboBoxGetSelected (cb, ls) = do
  sel  <- get cb comboBoxActive
  list <- listStoreToList ls
  if sel < 0 || length list <= sel
   then return Nothing
   else return $ Just $ list!!sel

typedComboBoxGetSelectedUnsafe :: TypedComboBox a -> IO a
typedComboBoxGetSelectedUnsafe (cb, ls) = do
  sel  <- get cb comboBoxActive
  list <- listStoreToList ls
  return $ list!!sel

typedComboBoxSetSelected :: (Eq a) => TypedComboBox a -> a -> IO ()
typedComboBoxSetSelected (cb, ls) x = do
  list <- listStoreToList ls
  case elemIndex x list of
   Nothing -> return ()
   Just ix -> set cb [ comboBoxActive := ix ]
