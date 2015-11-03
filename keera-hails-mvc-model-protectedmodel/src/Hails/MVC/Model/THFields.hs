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
protectedField :: String -> Q Type -> String -> String -> Q [Dec]
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
                             ftype
                       )
                       (conT (mkName pmodel))
                     )
                     (conT (mkName event))
       pmTo       = appT arrowT (conT (mkName "ProtectedModel"))
       typeToIO   = appT (appT arrowT ftype) ioNil
       ioNil      = appT (conT (mkName "IO")) (conT (mkName "()"))
       ioType     = appT (conT (mkName "IO")) ftype

       fnamelc    = lcFst fname

-- | Creates a setter and a getter that works at ReactiveModel level.
reactiveField :: String -> Q Type -> Q [Dec]
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
                       , conE (mkName (fname ++ "Changed"))           -- Event to trigger when changed
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
       getterType = appT rmTo ftype
       fieldType  = appT (conT (mkName "Field")) ftype
       rmTo       = appT arrowT (conT (mkName "ReactiveModel"))
       typeToRM   = appT (appT arrowT ftype)
                         (conT (mkName "ReactiveModel"))
       fnamelc    = lcFst fname

lcFst :: String -> String
lcFst []     = []
lcFst (x:xs) = (toLower x) : xs

-- | Creates a setter and a getter that works at ReactiveModel level.
-- using a specific event name
reactiveFieldE :: String -> String -> Q Type -> Q [Dec]
reactiveFieldE fname ename ftype = sequenceQ
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
                       , conE (mkName ename)           -- Event to trigger when changed
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
       getterType = appT rmTo ftype
       fieldType  = appT (conT (mkName "Field")) ftype
       rmTo       = appT arrowT (conT (mkName "ReactiveModel"))
       typeToRM   = appT (appT arrowT ftype)
                         (conT (mkName "ReactiveModel"))
       fnamelc    = lcFst fname

-- | Creates a setter and a getter that works at ReactiveModel level.
-- using a specific event name
reactiveFieldEvUndo :: String -> String -> Q Type -> Q [Dec]
reactiveFieldEvUndo fname ename ftype = sequenceQ
  -- Declare plain setter
  [ sigD setterName setterType
  , funD setterName [clause []
                     -- Main result: just use the field's setter
                     (normalB (appE (varE (mkName "fieldSetterUndo"))
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
                       , conE (mkName ename)           -- Event to trigger when changed
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
       getterType = appT rmTo ftype
       fieldType  = appT (conT (mkName "Field")) ftype
       rmTo       = appT arrowT (conT (mkName "ReactiveModel"))
       typeToRM   = appT (appT arrowT ftype)
                         (conT (mkName "ReactiveModel"))
       fnamelc    = lcFst fname
