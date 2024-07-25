{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List.NonEmpty ( NonEmpty((:|)) )
import Rust

exRust =
  Fn
    []
    PublicV
    "id"
    (FnDecl
      []
      (Just
        (ImplTrait
          (TraitTyParamBound
           (PolyTraitRef
              []
              (TraitRef
                (Path
                  False
                  [PathSegment
                    "Fn"
                    (Just
                      (Parenthesized
                        [PathTy
                          Nothing
                          (Path
                            False
                            [PathSegment
                              "Id00000000000000000000000000000000"
                              (Just
                                (AngleBracketed
                                  []
                                  [PathTy
                                    Nothing
                                    (Path
                                      False
                                      [PathSegment "A" Nothing ()]
                                      ()
                                    )
                                    ()
                                  ]
                                  []
                                  ()
                                )
                              )
                              ()
                            ]
                            ()
                          )
                          ()
                        ]
                        (Just (PathTy Nothing (Path False [PathSegment "A" Nothing ()] ()) ()))
                        ()
                      )
                    )
                    ()
                  ]
                  ()
                )
              )
              ()
            )
            None
            ()
            :| []
          )
          ()
        )
      )
      False
      ()
    )
    Normal
    NotConst
    Rust
    (Generics
      []
      [TyParam
        []
        "A"
        [TraitTyParamBound
          (PolyTraitRef [] (TraitRef (Path False [PathSegment "Clone" Nothing ()] ())) ())
          None
          ()
        ]
        Nothing
        ()
      ]
      (WhereClause [] ())
      ()
    )
    (Block
      [NoSemi
        (Closure
          []
          Movable
          Value
          (FnDecl [Arg (Just (IdentP (ByValue Immutable) "x" Nothing ())) (Infer ()) ()] Nothing False ())
          (PathExpr [] Nothing (Path False [PathSegment "x" Nothing ()] ()) ())
          ()
        )
        ()
      ]
      Normal
      ()
    )
    ()

main :: IO ()
main = printRust exRust

