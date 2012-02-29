-- | This module uses Template Haskell to declare getters and setters
--   for a given field and type that access the ProtectedModel in the
--   IO Monad and the reactive model.

module Hails.MVC.Model.THAccessors where

-- External imports
import Data.Char
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib

-- Creates a setter and a getter at ProtectedModel level that works in the IO Monad
protectedModelAccessors :: String -> String -> Q [Dec]
protectedModelAccessors fname ftype = sequenceQ
  -- Declare plain setter
  [ sigD setterName setterType
  , funD setterName [clause [varP (mkName "pm"), varP (mkName "n")] 
                     (normalB (appE (appE (varE (mkName "applyToReactiveModel"))
                                          (varE (mkName "pm"))
                                    )
                                    (infixE Nothing 
                                            (varE (mkName ("RM.set" ++ fname)))
                                            (Just (varE (mkName "n")))
                                    )
                              )
                     )
                     []
                     ]
  -- Declare plain getter
  , sigD getterName getterType
  , funD getterName [clause []
                     (normalB (infixE Nothing 
                                      (varE (mkName "onReactiveModel")) 
                                      (Just (varE (mkName ("RM.get" ++ fname))))
                              )
                     )
                     []
                     ]
  ]
 where setterName = mkName ("set" ++ fname)
       getterName = mkName ("get" ++ fname)
       setterType = appT pmTo typeToIO
       getterType = appT pmTo ioType
       pmTo       = appT arrowT (conT (mkName "ProtectedModel"))
       typeToIO   = appT (appT arrowT (conT (mkName ftype))) ioNil
       ioNil      = appT (conT (mkName "IO")) (conT (mkName "()"))
       ioType     = appT (conT (mkName "IO")) (conT (mkName ftype))

-- | Creates a setter and a getter that works at ReactiveModel level.
reactiveModelAccessors :: String -> String -> Q [Dec]
reactiveModelAccessors fname ftype = sequenceQ
  -- Declare plain setter
  [ sigD setterName setterType
  , funD setterName 
                    [clause 
                     -- Setter args: rm (reactive model), n (value)
                     [varP (mkName "rm"), varP (mkName "n")]
                     -- Main result: triggerEvent rm' ev
                     (normalB (appE (appE (varE (mkName "triggerEvent"))
                                          (varE (mkName "rm'"))
                                    )
                                    (varE (mkName "ev"))
                              )
                     )
                     -- Where rm' = updated rm
                     [valD (varP (mkName "rm'"))
                           (normalB (infixE (Just (varE (mkName "rm")))
                                            (varE (mkName "onBasicModel"))
                                            (Just (lamE [varP (mkName "b")]
                                                        (recUpdE (varE (mkName "b"))
                                                                 [fieldExp 
                                                                   (mkName fnamelc)
                                                                   (varE (mkName "n"))
                                                                 ]
                                                        )
                                                  )
                                            )
                                    )
                           )
                           []
                      -- Where ev = Corresponding Event
                      , valD (varP (mkName "ev"))
                             (normalB (conE (mkName (fname ++ "Changed"))))
                             []
                      ]
                    ]                     
  -- Declare plain getter
  , sigD getterName getterType
  , funD getterName [clause []
                     -- recordField . basicModel
                     (normalB (infixE (Just (varE (mkName fnamelc)))
                                      (varE (mkName ".")) 
                                      (Just (varE (mkName "basicModel")))
                              )
                     )
                     []
                     ]
  ]
 where setterName = mkName ("set" ++ fname)
       getterName = mkName ("get" ++ fname)
       setterType = appT rmTo typeToRM
       getterType = appT rmTo (conT (mkName ftype))
       rmTo       = appT arrowT (conT (mkName "ReactiveModel"))
       typeToRM   = appT (appT arrowT (conT (mkName ftype))) (conT (mkName "ReactiveModel"))
       fnamelc    = lcFst fname
       
lcFst :: String -> String         
lcFst []     = []
lcFst (x:xs) = (toLower x) : xs