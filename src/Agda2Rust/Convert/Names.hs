{-# LANGUAGE OverloadedStrings #-}
module Agda2Rust.Convert.Names where

import qualified Agda as A
import qualified Rust as R
import Rust.Utils

import Agda2Rust.Convert.Class ( (:~>) )

unqualR :: A.QName -> R.Ident
unqualR = R.mkIdent . A.unqual

unqualField :: A.QName -> A.QName -> R.Ident
unqualField recName recField = unqualR recName <> "·" <> unqualR recField

-- unqualR' :: A.QName -> C R.Ident
-- unqualR' n = ifJustM (isBuiltinTerm n) (return . compileBuiltinTerm) $ do
--   R.mkIdent $ unqual n
--   where

qualR :: A.QName :~> R.Path
qualR qn = do
  Right con <- fmap A.conName <$> A.getConHead qn
  ty  <- A.getConstructorData qn
  return $ RConRef (unqualR ty) (unqualR con)

parentQualR :: A.QName :~> R.Path
parentQualR qn = do
  con <- A.getConstructorData qn
  return $ RRef (unqualR con)
