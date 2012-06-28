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

-- | Accessor for Glade objects from Gtk Builders encapsulated in
-- Views, by name and -- type.
gtkViewAccessor :: String -> String -> String -> Q [Dec]
gtkViewAccessor uiAccessor name kind = sequenceQ
  -- Declaration
  [ sigD funcName
         -- Builder -> IO Kind
         (appT (appT arrowT (conT (mkName "View")))           
               (appT (conT (mkName "IO")) (conT (mkName kind))))
  -- Implementation
  , funD funcName                                                 
         -- castedOnBuilder objectName
         [clause [varP builderName]
                 (normalB (appE ((appE castedAccess                   
                                 (litE (stringL name)))
                                )
                                (appE (varE (mkName uiAccessor))
                                      (varE builderName)
                                )
                          )) []]
  ]

  where castedAccess = appE (varE (mkName "fromBuilder")) casting
        casting      = varE (mkName ("castTo" ++ kind))
        funcName     = mkName name
        builderName  = mkName "b"
