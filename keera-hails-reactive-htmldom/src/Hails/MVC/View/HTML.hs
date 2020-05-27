-- |
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Hails.MVC.View.HTML where

import Control.Applicative            ((<$>))
import Control.Monad                  (void)
import Control.Monad.IO.Class         (liftIO)
import Data.CBMVar                    (newCBMVar, writeCBMVar)
import Data.CBMVar.Reactive           (cbmvarReactiveRO, cbmvarReactiveRW)
import Data.ReactiveValue
import Data.String
import GHCJS.DOM.Element              (IsElement, setInnerHTML)
import GHCJS.DOM.EventM               (mouseButton, mouseClientXY, on)
import GHCJS.DOM.EventTargetClosures  (EventName, unsafeEventName)
import GHCJS.DOM.GlobalEventHandlers  (input, mouseDown, mouseMove)
import GHCJS.DOM.HTMLElement          (setInnerText)
import GHCJS.DOM.HTMLInputElement     (getValue, setValue)
import GHCJS.DOM.HTMLParagraphElement (HTMLParagraphElement (..))
import GHCJS.DOM.Types                (HTMLElement, HTMLInputElement (..),
                                       IsGlobalEventHandlers, IsHTMLElement,
                                       MouseEvent, toHTMLElement, toJSString)

-- * GHCJS DOM Reactive API
inputTextReactive :: HTMLInputElement -> ReactiveFieldReadWrite IO String
inputTextReactive x = ReactiveFieldReadWrite setter getter notifier
 where setter = setValue x
       getter = getValue x
       notifier p = void $ on x input (liftIO p)

htmlElementClick :: IsHTMLElement e => e -> ReactiveFieldRead IO ()
htmlElementClick e = ReactiveFieldRead (return ()) (\p -> void $ on e' click (liftIO p))
  where
    e' = toHTMLElement e

click :: EventName HTMLElement MouseEvent
click = unsafeEventName (toJSString "click")

paragraphTextReactive :: HTMLParagraphElement -> ReactiveFieldWrite IO String
paragraphTextReactive x = ReactiveFieldWrite setter
 where setter = setInnerHTML x

mouseDownReactive :: (IsGlobalEventHandlers a, IsElement a) => a -> ReactiveFieldRead IO ()
mouseDownReactive s = eventR $ \p -> void $ on s mouseDown (liftIO p)

mousePosElementReactive :: (IsGlobalEventHandlers e, IsElement e) => e -> IO (ReactiveFieldRead IO (Int, Int))
mousePosElementReactive e = do
  p <- newCBMVar (0,0)
  void $ on e mouseMove (mouseClientXY >>= \pos -> liftIO (writeCBMVar p pos))
  return (cbmvarReactiveRO p)
