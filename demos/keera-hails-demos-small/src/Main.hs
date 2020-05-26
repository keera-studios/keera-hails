import GHCJS.DOM          (currentDocumentUnchecked)
import GHCJS.DOM.Document (getBodyUnchecked)
import GHCJS.DOM.Element  (setInnerHTML)

main :: IO ()
main = do
  doc  <- currentDocumentUnchecked
  body <- getBodyUnchecked doc
  setInnerHTML body "Hello Haskell!"
