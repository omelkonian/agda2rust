{-# LANGUAGE FlexibleInstances #-}
module Agda2Rust.Convert.Constructors () where

import Utils

import qualified Agda as A
import Agda.Utils

import qualified Rust as R
import Rust.Utils

import Agda2Rust.Monad
import Agda2Rust.Convert.Class
import Agda2Rust.Convert.Names
import Agda2Rust.Convert.Types

-- | Compiling Agda constructors into Rust variants.
instance (A.QName, A.Type) ~> R.Variant where
  go (c, ty) = inConstructor c $ do
    report $ "** compiling constructor: " <> pp c
    as <- argTys ty
    -- report $ " as: " <> pp as
    fs <- goFs 0 as
    setArity c (length fs)
    return $ RVariant (unqualR c) fs
    where
      goFs :: Int -> Tel :~>* R.StructField
      goFs _ [] = return []
      goFs i (a@(A.unDom -> (x, ty)):fs) = do
        let ctx = A.addContext a
        keep <- isKeptArg <$> classifyArg a
        if keep then
          (:) <$> inArgument i (goF (x, ty))
              <*> ctx (goFs (i + 1) fs)
        else
          ctx (goFs i fs)

      goF :: (String, A.Type) :~> R.StructField
      goF (x, ty) = RField <$> go ty


