module Language.Haskell.TH.DeriveField where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib

deriveField :: String -> String -> String -> Name -> Q [Dec]
deriveField cls clsOp fld name = sequenceQ
 [ instanceD (cxt []) (appT (conT (mkName cls)) (conT name))
   [ funD (mkName clsOp) [ clause [] (normalB (varE (mkName fld))) [] ] ]
 ]
