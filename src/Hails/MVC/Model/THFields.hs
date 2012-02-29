-- | This module uses Template Haskell to declare reactive fields for
--   a given model field and type that access the ProtectedModel in
--   the IO Monad and the reactive model.

module Hails.MVC.Model.THFields where

-- External imports
import Data.Char
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib

-- | Creates a setter and a getter that works at ProtectedModel level
-- inside the IO Monad
protectedField :: String -> String -> String -> String -> Q [Dec]
protectedField fname ftype pmodel event = sequenceQ
  -- Declare plain field
  [ sigD setterName setterType
  , funD setterName [clause []
                     -- Main result: setter field
                     (normalB (appE (varE (mkName "reSetter"))
                                    (varE fieldName)
                              )
                     )
                     -- where
                     []
                    ]                     
  -- Declare plain getter
  , sigD getterName getterType
  , funD getterName [clause []
                     -- Main result: getter field
                     (normalB (appE (varE (mkName "reGetter"))
                                    (varE fieldName)
                              )
                     )
                     []
                     ]
  -- Declare protected field
  , sigD fieldName fieldType
  , funD fieldName [clause []
                     (normalB 
                      (recConE (mkName "ReactiveElement")
                               [fieldExp
                                 (mkName "reEvents")
                                 (listE [conE (mkName 
                                                ("RM." ++ fname ++ "Changed"))
                                        ]
                                 )
                               , fieldExp
                                 (mkName "reSetter")
                                 (lamE [varP (mkName "pm")
                                       , varP (mkName "c")
                                       ] 
                                    (infixE 
                                     (Just (varE (mkName "pm")))
                                     (varE (mkName "applyToReactiveModel"))
                                     (Just (infixE Nothing
                                            (varE (mkName 
                                                   ("RM." ++ "set" ++ fname)
                                                  )
                                            )
                                            (Just (varE (mkName "c")))
                                           )
                                     )
                                    )
                                 )
                               , fieldExp (mkName "reGetter")
                                          (infixE 
                                            Nothing
                                            (varE (mkName "onReactiveModel"))
                                            (Just (varE (mkName
                                                          ("RM." ++ "get" ++ fname)))
                                            )
                                          )
                               ]
                      )
                     )
                    []
                   ]
  ]
 where setterName = mkName ("set" ++ fname)
       getterName = mkName ("get" ++ fname)
       fieldName  = mkName (fnamelc ++ "Field")
       setterType = appT pmTo typeToIO
       getterType = appT pmTo ioType
       fieldType  = appT
                     (appT
                       (appT (conT (mkName "ReactiveElement")) 
                             (conT (mkName ftype))
                       )
                       (conT (mkName pmodel))
                     )
                     (conT (mkName event))
       pmTo       = appT arrowT (conT (mkName "ProtectedModel"))
       typeToIO   = appT (appT arrowT (conT (mkName ftype))) ioNil
       ioNil      = appT (conT (mkName "IO")) (conT (mkName "()"))
       ioType     = appT (conT (mkName "IO")) (conT (mkName ftype))
                    
       fnamelc    = lcFst fname

-- | Creates a setter and a getter that works at ReactiveModel level.
reactiveField :: String -> String -> Q [Dec]
reactiveField fname ftype = sequenceQ
  -- Declare plain setter
  [ sigD setterName setterType
  , funD setterName [clause []
                     -- Main result: just use the field's setter
                     (normalB (appE (varE (mkName "fieldSetter"))
                                    (varE fieldName)
                              )
                     )
                     -- where
                     []
                    ]                     
  -- Declare plain getter
  , sigD getterName getterType
  , funD getterName [clause []
                     -- Main result: just use the field's getter
                     (normalB (appE (varE (mkName "fieldGetter"))
                                    (varE fieldName)
                              )
                     )
                     []
                     ]
  -- Declare field with 4 elements 
  , sigD fieldName fieldType
  , funD fieldName [clause []
                     (normalB 
                      (tupE
                       [ varE (mkName fnamelc)                        -- function to read from model
                       , varE (mkName "preTrue")                      -- precondition to update model
                       , lamE [varP (mkName "v"), varP (mkName "b")]  -- function to update model
                         (recUpdE (varE (mkName "b"))
                          [fieldExp 
                           (mkName fnamelc)
                           (varE (mkName "v"))
                          ]
                         )
                       , (conE (mkName (fname ++ "Changed")))           -- Event to trigger when changed
                       ]
                      )
                     )
                     []
                    ]
  ]
 where setterName = mkName ("set" ++ fname)
       getterName = mkName ("get" ++ fname)
       fieldName  = mkName (fnamelc ++ "Field")
       setterType = appT rmTo typeToRM
       getterType = appT rmTo (conT (mkName ftype))
       fieldType  = appT (conT (mkName "Field")) (conT (mkName ftype))
       rmTo       = appT arrowT (conT (mkName "ReactiveModel"))
       typeToRM   = appT (appT arrowT (conT (mkName ftype))) 
                         (conT (mkName "ReactiveModel"))
       fnamelc    = lcFst fname
       
lcFst :: String -> String         
lcFst []     = []
lcFst (x:xs) = (toLower x) : xs
