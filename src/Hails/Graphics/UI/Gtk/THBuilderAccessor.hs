module Hails.Graphics.UI.Gtk.THBuilderAccessor where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib

gtkBuilderAccessor :: String -> String -> Q [Dec]
gtkBuilderAccessor name kind = sequenceQ
 -- Signature: <accessor> :: Builder -> IO <WidgetType>
 [ sigD (mkName name)
        (appT (appT arrowT (conT (mkName "Builder")))
              (appT (conT (mkName "IO")) (conT (mkName kind)))
        )
 -- Implementation: <accessor> :: fromBuilder castTo<WidgetType> "<accessor>"
 , funD (mkName name)
        [ clause [] 
                 -- Just apply the casting operation and the name to the
                 -- builder accessor
                 (normalB
                   (appE                                         
                     (appE (varE (mkName "fromBuilder"))           
                           (varE (mkName ("castTo" ++ kind))))   
                     (litE (stringL name))                       
                   )                                             
                 )                                               
                 []
        ]
 ]
