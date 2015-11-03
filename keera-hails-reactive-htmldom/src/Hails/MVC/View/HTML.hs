module Hails.MVC.View.HTML where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)
import Data.Maybe (fromJust)
import Data.CBMVar
import Data.ReactiveValue
import GHCJS.DOM                      (enableInspector, webViewGetDomDocument, runWebGUI)
import GHCJS.DOM.Document             (getBody, createElement, click, getElementById)
import GHCJS.DOM.Element              (setInnerHTML, IsElement, mouseDown, mouseMove, input)
import GHCJS.DOM.EventM               (on, mouseClientXY)
import GHCJS.DOM.HTMLDivElement       (castToHTMLDivElement)
import GHCJS.DOM.HTMLElement          (setInnerText)
import GHCJS.DOM.HTMLInputElement     (getValue, setValue)
import GHCJS.DOM.HTMLParagraphElement (castToHTMLParagraphElement, HTMLParagraphElement(..))
import GHCJS.DOM.Node                 (appendChild)
import GHCJS.DOM.Types                (HTMLInputElement(..) , castToHTMLInputElement)


-- * GHCJS DOM Reactive API
inputTextReactive :: HTMLInputElement -> ReactiveFieldReadWrite IO String
inputTextReactive x = ReactiveFieldReadWrite setter getter notifier
 where setter = setValue x . Just
       getter = fromJust <$> getValue x
       notifier p = void $ on x input (liftIO p)

paragraphTextReactive :: HTMLParagraphElement -> ReactiveFieldWrite IO String
paragraphTextReactive x = ReactiveFieldWrite setter
 where setter = setInnerHTML x . Just

mouseDownReactive :: IsElement a => a -> ReactiveFieldRead IO ()
mouseDownReactive s = eventR $ \p -> void $ on s mouseDown (liftIO p)

mousePosElementReactive :: IsElement e => e -> IO (ReactiveFieldRead IO (Int, Int))
mousePosElementReactive e = do
  p <- newCBMVar (0,0)
  void $ on e mouseMove (mouseClientXY >>= \pos -> liftIO (writeCBMVar p pos))
  return (cbmvarReactiveRO p)

cbmvarReactiveRO :: CBMVar a -> ReactiveFieldRead IO a
cbmvarReactiveRO mvar = ReactiveFieldRead getter notifier
  where getter     = readCBMVar mvar
        notifier n = installCallbackCBMVar mvar n
