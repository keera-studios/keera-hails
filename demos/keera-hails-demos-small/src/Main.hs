{-# LANGUAGE TemplateHaskell #-}

-- External imports
import Data.FileEmbed (embedStringFile)
import Data.String    (IsString)

-- External imports: GHCJS
import GHCJS.DOM          (currentDocumentUnchecked)
import GHCJS.DOM.Document (getBodyUnchecked, getHeadUnchecked)
import GHCJS.DOM.Element  (setInnerHTML)

main :: IO ()
main = do
  doc <- currentDocumentUnchecked
  header <- getHeadUnchecked doc
  setInnerHTML header (staticHeader :: String)

  body <- getBodyUnchecked doc
  setInnerHTML body (staticBody :: String)

-- | Static partial loaded from file
staticHeader :: IsString a => a
staticHeader = $(embedStringFile "data/head.html")

-- | Static partial loaded from file.
staticBody :: IsString a => a
staticBody = $(embedStringFile "data/body.html")
