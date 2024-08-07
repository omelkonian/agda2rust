{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Maybe ( fromMaybe, catMaybes )
import qualified Data.Map as M ( lookup )
import Data.Function ( on )
import Data.List ( sortBy, intercalate )
import qualified Data.Text as T

import Text.Read ( readMaybe )

import Control.Monad ( unless )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.DeepSeq ( NFData(..) )

import System.FilePath ( takeDirectory, (</>) )
import System.Directory ( createDirectoryIfMissing )
import System.Environment ( getArgs )
import System.Console.GetOpt
  ( OptDescr(Option), ArgDescr(ReqArg, NoArg), ArgOrder(Permute), getOpt )

import Data.Version ( showVersion )
import Paths_agda2rust ( version )

import Agda.Lib
  ( Range(..), rStart, posLine
  , qnameName, qnameModule
  , Ranged(..), Origin(..), getOrigin
  , TopLevelModuleName, moduleNameToFileName
  , Definition(..)
  , TCM, withCurrentModule, iInsideScope, setScope
  , CompilerPragma(..), getUniqueCompilerPragma
  , runAgda
  , curIF, compileDir
  , Backend(..), Backend'(..), Recompile(..), IsMain, nameBindingSite
  , iForeignCode, getForeignCodeStack, ForeignCode(..)
  , Flag
  )
import Agda.Utils

import qualified Language.Rust.Pretty as R

import Utils ( report )
import Agda2Rust ( convert, RDef(..), runC, runC0, initState, State )

-- | State propagated across modules.
stateFile :: FilePath
stateFile = "agda2rust.state"

cleanupState :: IO ()
cleanupState = writeState initState

readState :: IO State
readState = read <$> readFile stateFile

writeState :: State -> IO ()
writeState = writeFile stateFile . show

--

main :: IO ()
main = do
  isInt <- isInteractive
  cleanupState
  if isInt
    then runAgda [Backend backend{isEnabled = const False}]
    else runAgda [Backend backend]

isInteractive :: IO Bool
isInteractive = do
  let o = Option ['I'] ["interactive", "interaction", "interaction-json"] (NoArg ()) ""
  (i, _, _) <- getOpt Permute [o] <$> getArgs
  return $ not $ null i

data Options = Options
  { optOutDir  :: Maybe FilePath
  , optEnabled :: Bool
  }

instance NFData Options where
  rnf _ = ()

outdirOpt :: FilePath -> Flag Options
outdirOpt dir opts = return opts{ optOutDir = Just dir }

disableOpt :: Flag Options
disableOpt opts = return opts { optEnabled = False }

defaultOptions :: Options
defaultOptions = Options
  { optOutDir  = Nothing
  , optEnabled = True
  }

type ModuleEnv = ()
type ModuleRes = ()
type CompiledDef = Ranged String

renderCode :: [CompiledDef] -> String
renderCode = unlines . map rangedThing . sortBy (compare `on` rLine . rangeOf)
  where rLine :: Range -> Int
        rLine r = fromIntegral $ fromMaybe 0 $ posLine <$> rStart r

backend :: Backend' Options Options ModuleEnv ModuleRes (Maybe CompiledDef)
backend = Backend'
  { backendName           = "agda2rust"
  , backendVersion        = Just (showVersion version)
  , options               = defaultOptions
  , commandLineFlags      =
      [ Option ['o'] ["out-dir"] (ReqArg outdirOpt "DIR")
        "Write output files to DIR. (default: project root)"
      , Option ['d'] ["disable"] (NoArg disableOpt)
          "Disable backend and fall back to vanilla Agda behaviour, \
          \without compilation (important for Emacs mode). \
          \Implied when run in interactive mode (with --interactive, --interaction or --interaction-json)."
      ]
  , isEnabled             = optEnabled
  , preCompile            = return
  , postCompile           = \ _ _ _ -> return ()
  , preModule             = moduleSetup
  , postModule            = writeModule
  , compileDef            = compile
  , scopeCheckingSuffices = False
  , mayEraseType          = \qn -> do
    return True
    -- return False
    -- return $ getOrigin qn /= UserWritten
  }

moduleSetup :: Options -> IsMain -> TopLevelModuleName -> Maybe FilePath
            -> TCM (Recompile ModuleEnv ModuleRes)
moduleSetup _ _ m _ = do
  setScope . iInsideScope =<< curIF
  return $ Recompile ()

defRange :: Definition -> Range
defRange = nameBindingSite . qnameName . defName

compile :: Options -> ModuleEnv -> IsMain -> Definition -> TCM (Maybe CompiledDef)
compile opts tlm _ def@(Defn{..}) = withCurrentModule (qnameModule defName) $ do
  s <- liftIO readState
  (mdef, s') <- runC s (convert def)
  liftIO $ writeState s'
  return $ case mdef of
    IgnoreDef       -> Nothing
    CompileDef cdef -> Just $ Ranged (defRange def) $ show (R.pretty' cdef)

getForeignRust :: TCM [CompiledDef]
getForeignRust
  = reverse
  . fmap (\case ForeignCode r s -> Ranged r s)
  . fromMaybe []
  . fmap getForeignCodeStack
  . M.lookup "AGDA2RUST"
  . iForeignCode <$> curIF

ignoredRustWarnings, experimentRustFeatures :: [String]
ignoredRustWarnings =
  [ "dead_code"
  , "non_snake_case"
  , "unused_variables"
  , "non_camel_case_types"
  , "non_upper_case_globals"
  , "unreachable_patterns"
  -- , "uncommon_codepoints" -- only crate-level attribute
  ]
experimentRustFeatures =
  -- []
  [ "type_alias_impl_trait"
  , "impl_trait_in_fn_trait_return"
  , "tuple_trait"
  , "unboxed_closures"
  , "fn_traits"

  , "const_trait_impl"
  , "effects"
  ]

writeModule :: Options -> ModuleEnv -> IsMain -> TopLevelModuleName
            -> [Maybe CompiledDef] -> TCM ModuleRes
writeModule opts _ _ m (catMaybes -> cdefs) = do
  pragmas <- getForeignRust
  outDir <- compileDir
  let code    = renderCode (pragmas <> addNewLines cdefs)
      rustFn  = moduleNameToFileName m "rs"
      outFile = fromMaybe outDir (optOutDir opts) <> "/" <> rustFn
      outS =  mkDirective "feature" experimentRustFeatures
           <> mkDirective "allow" ignoredRustWarnings
           <> "\n"
           <> includeRTS
           <> "\n"
           <> fixCode code
  runC0 $ report $ "\n************** MODULE: " <> rustFn <> " ***************\n"
        <> outS
        <> "************************************************************"
  unless (null code) $
    writeRsFile outFile outS
  where
    writeRsFile :: FilePath -> String -> TCM ()
    writeRsFile outFn content = liftIO $ do
      let outDir = takeDirectory outFn
      createDirectoryIfMissing True outDir
      writeFile outFn content

    addNewLines :: [CompiledDef] -> [CompiledDef]
    addNewLines [] = []
    addNewLines (d:ds) = d : Ranged (rangeOf d) "" : addNewLines ds

    mkDirective group items
      | null items = ""
      | otherwise  = "#![" <> group <> "(" <> intercalate "," items <> ")]\n"

    includeRTS = "use unicurry::*;\n"

    -- c.f. test/RustPrintBug
    fixCode :: String -> String
    fixCode = T.unpack . T.replace " + {\n" " {\n" . T.pack
