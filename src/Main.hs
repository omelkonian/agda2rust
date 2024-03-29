module Main where

import Data.Maybe ( fromMaybe )
import qualified Data.Map as M ( lookup )
import Data.Function ( on )
import Data.List ( sortBy, intercalate )

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

import Agda.Syntax.Position ( Range(..), rStart, posLine )
import Agda.Syntax.Common ( Ranged(..) )
import Agda.Syntax.Internal ( qnameName, qnameModule )
import Agda.Syntax.TopLevelModuleName
  ( TopLevelModuleName, moduleNameToFileName )

import Agda.Compiler.Common ( curIF, compileDir )
import Agda.Compiler.Backend
  ( Backend(..), Backend'(..), Recompile(..), IsMain, nameBindingSite
  , iForeignCode, getForeignCodeStack, ForeignCode(..)
  , Flag
  )

import Agda.TypeChecking.Monad.Base ( Definition(..) )
import Agda.TypeChecking.Monad
  ( TCM, withCurrentModule, iInsideScope, setScope
  , CompilerPragma(..), getUniqueCompilerPragma )

import Agda.Main ( runAgda )

import qualified Language.Rust.Pretty as R

import Agda2Rust
  ( convert, ignoreDef, report, ppm, runC, runC0, initState, State )

--

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

backend :: Backend' Options Options ModuleEnv ModuleRes CompiledDef
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
  , mayEraseType          = \ _ -> return True
  }

moduleSetup :: Options -> IsMain -> TopLevelModuleName -> Maybe FilePath
            -> TCM (Recompile ModuleEnv ModuleRes)
moduleSetup _ _ m _ = do
  setScope . iInsideScope =<< curIF
  return $ Recompile ()

defRange :: Definition -> Range
defRange = nameBindingSite . qnameName . defName

compile :: Options -> ModuleEnv -> IsMain -> Definition -> TCM CompiledDef
compile opts tlm _ def@(Defn{..})
  | ignoreDef theDef
  = return $ Ranged (defRange def) ""
  | otherwise
  -- $ getUniqueCompilerPragma "AGDA2RUST" defName >>= \case
  --     Nothing -> return []
  --     Just (CompilerPragma _ _) -> ...
  = withCurrentModule (qnameModule defName) $ do
    s <- liftIO readState
    (cdef, s') <- runC s (convert def)
    liftIO $ writeState s'
    return $ Ranged (defRange def) $ show (R.pretty' cdef)

getForeignRust :: TCM [CompiledDef]
getForeignRust
  = reverse
  . fmap (\case ForeignCode r s -> Ranged r s)
  . fromMaybe []
  . fmap getForeignCodeStack
  . M.lookup "AGDA2RUST"
  . iForeignCode <$> curIF

ignoredRustWarnings :: [String]
ignoredRustWarnings =
  [ "dead_code"
  , "non_snake_case"
  , "unused_variables"
  , "non_camel_case_types"
  -- , "uncommon_codepoints" -- only crate-level attribute
  ]

writeModule :: Options -> ModuleEnv -> IsMain -> TopLevelModuleName
            -> [CompiledDef] -> TCM ModuleRes
writeModule opts _ _ m cdefs = do
  pragmas <- getForeignRust
  outDir <- compileDir
  let code    = renderCode (pragmas <> cdefs)
      rustFn  = moduleNameToFileName m "rs"
      outFile = fromMaybe outDir (optOutDir opts) <> "/" <> rustFn
      outS =  "#![allow(" <> intercalate "," ignoredRustWarnings <> ")]\n"
           <> code
  runC0 $ report $ "\n******* MODULE: " <> rustFn <> "********\n"
        <> outS
  unless (null code) $
    writeRsFile outFile outS
  where
    writeRsFile :: FilePath -> String -> TCM ()
    writeRsFile outFn content = liftIO $ do
      let outDir = takeDirectory outFn
      createDirectoryIfMissing True outDir
      writeFile outFn content
