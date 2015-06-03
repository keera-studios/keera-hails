module Graphics.UI.Gtk.Reactive.TreeView where

import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)
import Data.ReactiveValue
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Helpers.TreeView

-- treeViewSelectedRowsReactive :: TreeView -> ReactiveFieldRead IO [TreePath]
-- treeViewSelectedRowsReactive tv = ReactiveFieldRead getter notifier
--  where getter = treeSelectionGetSelectedRows =<< treeViewGetSelection tv
--        notifier p = void (tv `on` cursorChanged $ liftIO p)

treeViewSelectedRowsReactive :: TreeView -> ReactiveFieldRead IO [TreePath]
treeViewSelectedRowsReactive tv = ReactiveFieldRead getter notifier
 where getter = treeViewGetSelectedPath tv
       notifier p = void (tv `on` cursorChanged $ liftIO p)

treeViewGetSelectedReactive :: TreeView -> ListStore a -> ReactiveFieldRead IO (Maybe a)
treeViewGetSelectedReactive tv ls = ReactiveFieldRead getter notifier
 where getter = treeViewGetSelected tv ls
       notifier p = void (tv `on` cursorChanged $ liftIO p)

