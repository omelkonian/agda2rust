module Agda2Rust.Pragma where

import Data.List ( stripPrefix )

import Agda
-- import Agda2Rust.Monad

-- | The pragma name of the AGDA2RUST backend.
pragmaName :: String
pragmaName = "AGDA2RUST"

-- | Types of AGDA2RUST pragmas.
data Pragma
  = FFI (Maybe PragmaQualifier) String
  -- ^ do not compile, instead use FFI
  -- {-# COMPILE AGDA2RUST <AgdaName> [const|static] = <RustName> #-}
  | NoFFI PragmaQualifier
  -- ^ qualify this definition
  -- {-# COMPILE AGDA2RUST <AgdaName> const|static #-}
  | Ignore
  -- ^ just ignore this definition
  -- {-# COMPILE AGDA2RUST <AgdaName> ignore #-}
  deriving (Eq, Show)

isNoFFI :: Pragma -> Bool
isNoFFI = \case {NoFFI _ -> True; _ -> False}

-- | Some pragma forms can be declared constant or static.
data PragmaQualifier = Const | Static
  deriving (Eq, Show, Read)

-- | Retrieve the pragma associated to a definition.
getRustPragma :: QName -> TCM (Maybe Pragma)
getRustPragma qn = do
  mpr <- getUniqueCompilerPragma pragmaName qn
  return $ case mpr of
    Just (CompilerPragma _ s) -> Just (parsePragma s)
    Nothing -> Nothing
  where
  parsePragma :: String -> Pragma
  parsePragma = \case
    "ignore" -> Ignore
    "const"  -> NoFFI Const
    "static" -> NoFFI Static
    s | Just s' <- stripPrefix "const = " s  -> FFI (Just Const) s'
    s | Just s' <- stripPrefix "static = " s -> FFI (Just Static) s'
    s | Just s' <- stripPrefix "= " s        -> FFI Nothing s'
    s -> panic "pragma" s

-- ** useful pattern synonyms
pattern ConstNoFFI  = Just (NoFFI Const)
pattern StaticNoFFI = Just (NoFFI Static)
