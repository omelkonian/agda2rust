{-# LANGUAGE OverloadedStrings #-}
module Agda2Rust.Convert.Names where

import Data.List ( intercalate )

import qualified Agda as A
import qualified Rust as R
import Rust.Utils

import Agda2Rust.Convert.Class ( (:~>) )

unqualR :: A.QName -> R.Ident
unqualR = R.mkIdent . A.unqual

unqualNoNameR :: A.QName -> R.Ident
unqualNoNameR qn =
  let ns  = map (\case{A.NameId n m -> show n} . A.nameId) $ filter A.isNoName (A.qnameToList0 qn)
      pre = "Where·" <> intercalate "·" ns <> "·"
  in R.mkIdent pre <> unqualR qn

unqualField :: A.QName -> A.QName -> R.Ident
unqualField recName recField = unqualR recName <> "·" <> unqualR recField

-- unqualR' :: A.QName -> C R.Ident
-- unqualR' n = ifJustM (isBuiltinTerm n) (return . compileBuiltinTerm) $ do
--   R.mkIdent $ unqual n
--   where

isAnonymousName :: A.QName -> Bool
isAnonymousName = any A.isNoName . A.qnameToList0

defNameR :: A.Definition -> R.Ident
defNameR A.Defn{..}
  | Just (recName, recField) <- A.isRecordProjection theDef
  = unqualField recName recField
  | isAnonymousName defName
  = unqualNoNameR defName
  | otherwise
  = unqualR defName

qualR :: A.QName :~> R.Path
qualR qn = do
  Right con <- fmap A.conName <$> A.getConHead qn
  ty <- A.getConstructorData qn
  return $ RConRef (unqualR ty) (unqualR con)

parentQualR :: A.QName :~> R.Path
parentQualR qn = do
  con <- A.getConstructorData qn
  return $ RRef (unqualR con)
