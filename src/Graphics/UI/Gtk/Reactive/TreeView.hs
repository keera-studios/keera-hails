module Graphics.UI.Gtk.Reactive.TreeView where

import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Graphics.UI.Gtk
import Data.ReactiveValue
import Data.Word
import Graphics.UI.Gtk.Reactive.Property

treeViewSelectedRowsReactive :: TreeView -> ReactiveFieldRead IO [TreePath]
treeViewSelectedRowsReactive tv = ReactiveFieldRead getter notifier
 where getter = do ts <- treeViewGetSelection tv
                   treeSelectionGetSelectedRows ts
       notifier p = void (tv `on` cursorChanged $ liftIO p)
