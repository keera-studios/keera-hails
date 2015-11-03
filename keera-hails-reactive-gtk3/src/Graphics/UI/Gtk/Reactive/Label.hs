-- | Publishes the main elements of an entry as reactive fields
module Graphics.UI.Gtk.Reactive.Label where

import Control.Applicative
import Graphics.Rendering.Pango as Pango
import Graphics.UI.Gtk
import Data.ReactiveValue

labelTextReactive :: (LabelClass l) => l -> ReactiveFieldReadWrite IO String
labelTextReactive l = ReactiveFieldReadWrite setter getter notifier
 where getter     = get l labelText
       setter v   = postGUIAsync $ set l [labelText := v]
       notifier _ = return ()

labelBackground :: Label -> ReactiveFieldReadWrite IO Pango.Color
labelBackground lbl = ReactiveFieldReadWrite setter getter (const (return ()))
 where setter x = postGUIAsync $ labelSetAttributes lbl [AttrBackground 0 (-1) x]
       getter   = postGUISync $ (\ls -> head [ x | AttrBackground _ _ x <- ls])
                                <$> labelGetAttributes lbl
