{-# LANGUAGE OverloadedStrings #-}
module Agda2Rust.Convert.FFI
  ( compileFFITy
  , compileFFIConst
  , compileFFIHead
  ) where

import Control.Monad ( unless, void )
import Data.Maybe ( isNothing )

import qualified Agda as A
import Agda ( panic )

import qualified Rust as R
import Rust.Utils

import Agda2Rust.Pragma
import Agda2Rust.Monad
import Agda2Rust.Convert.Class

-- ** Parsing Rust expressions/types/etc.
parseRust :: forall f. (Functor f, R.Parse (f R.Span))
          => String -> String -> f ()
parseRust ty s = do
  let res = R.parse @(f R.Span) (R.inputStreamFromString s)
  case res of
    Left _  -> panic ("cannot parse rust " <> ty <> ":") s
    Right e -> void e

-- ** FFI types
compileFFITy :: A.Type -> [A.Arg A.Term]
             -> (Maybe PragmaQualifier, String) :~> R.Ty
compileFFITy ty as (mq, s) = do
  unless (null as) $
    panic "FFI type (with non-null type parameters)" ty
  unless (isNothing mq) $
    panic "FFI type (with pragma qualifier)" ty
  return $ parseRust @R.Ty "type" s

-- ** FFI constants
compileFFIConst :: String -> R.Expr ()
compileFFIConst = parseRust @R.Expr "expression"

-- ** FFI functions
compileFFIHead :: (Maybe PragmaQualifier, String)
               -> (Maybe A.QName, ([R.Ty ()] -> [R.Expr ()] -> R.Expr ()))
compileFFIHead (mq, s) = (Nothing, f)
  where
    f [] [] | Just Const <- mq = compileFFIConst s
    f ps xs = rCall' (R.mkIdent s) ps xs


