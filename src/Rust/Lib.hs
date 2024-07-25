-- | Re-exporting stuff from `language-rust`.
module Rust.Lib
  ( module Language.Rust.Syntax
  , module Language.Rust.Data.Ident
  , module Language.Rust.Data.Position
  , module Language.Rust.Parser
  , module Language.Rust.Pretty
  , RustPretty, printRust
  ) where

import Language.Rust.Syntax
import Language.Rust.Data.Ident
import Language.Rust.Data.Position
import Language.Rust.Parser
import Language.Rust.Pretty

-- ** pretty printing Rust expressions/statements/types/etc.
import Text.Show.Pretty ( pPrint )

type RustPretty a = (Show a, Resolve a, Pretty a)

-- | Printing a rust construct on screen in two ways:
--     (1) the pretty-printed version
--     (2) a verbose `show` version for debugging
printRust :: RustPretty a => a -> IO ()
printRust src = hline >> print (pretty' src) >> hline >> pPrint src >> hline
  where hline = putStrLn "----------------------"
