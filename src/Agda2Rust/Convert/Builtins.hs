{-# LANGUAGE OverloadedStrings #-}
module Agda2Rust.Convert.Builtins
  ( isBuiltinDef
  , BuiltinTy(..), isBuiltinTy, compileBuiltinTy
  , BuiltinTerm(..), isBuiltinTerm, compileBuiltinTerm
  ) where

import Control.Monad ( unless )

import qualified Agda as A
import Agda ( ifM, panic )
import Agda.Builtins

import qualified Rust as R
import Rust.Utils

import Agda2Rust.Monad
import Agda2Rust.Convert.Class

-- ** builtin types
data BuiltinTy = Nat | Float | Char | String | Bool | Int

compileBuiltinTy :: A.Type -> [A.Arg A.Term] -> BuiltinTy -> C (R.Ty ())
compileBuiltinTy ty as b = do
  unless (null as) $ panic "primitive type (with non-null type parameters)" ty
  return $ RTyRef $ case b of
    Nat    -> "i32"
    Float  -> "f64"
    Char   -> "char"
    String -> "String"
    Bool   -> "bool"
    Int    -> "i32"

isBuiltinTy ::
  (A.HasBuiltins m, A.MonadReduce m) => A.QName -> m (Maybe BuiltinTy)
isBuiltinTy n
  = builtinNat     .-> Nat
  $ builtinFloat   .-> Float
  $ builtinChar    .-> Char
  $ builtinString  .-> String
  $ builtinBool    .-> Bool
  $ builtinInteger .-> Int
  $ return Nothing
  where (.->) def ty = ifM (A.isBuiltin n def) (return $ Just ty)

-- ** builtin terms
compileBuiltinTerm :: BuiltinTerm -> R.Expr ()
compileBuiltinTerm = \case
  TrueE  -> RLitTrue
  FalseE -> RLitFalse

data BuiltinTerm = TrueE | FalseE

isBuiltinTerm ::
  (A.HasBuiltins m, A.MonadReduce m) => A.QName -> m (Maybe BuiltinTerm)
isBuiltinTerm n
  = builtinTrue   .-> TrueE
  $ builtinFalse  .-> FalseE
  $ return Nothing
  where (.->) def ty = ifM (A.isBuiltin n def) (return $ Just ty)

