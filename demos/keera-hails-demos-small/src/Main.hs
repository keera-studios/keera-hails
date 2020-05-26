import GHCJS.DOM                  (currentDocumentUnchecked)
import GHCJS.DOM.Document         (createElement, getBodyUnchecked)
import GHCJS.DOM.HTMLInputElement (HTMLInputElement (..))
import GHCJS.DOM.Node             (appendChild_)
import GHCJS.DOM.Types            (Element (..), uncheckedCastTo)

import Data.ReactiveValue  ((=:>))
import Hails.MVC.View.HTML (inputTextReactive)

main :: IO ()
main = do
  doc  <- currentDocumentUnchecked
  body <- getBodyUnchecked doc

  input1 <- uncheckedCastTo HTMLInputElement <$> createElement doc "input"
  appendChild_ body input1

  input2 <- uncheckedCastTo HTMLInputElement <$> createElement doc "input"
  appendChild_ body input2

  inputTextReactive input1 =:> inputTextReactive input2
