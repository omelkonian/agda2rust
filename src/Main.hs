module Main where

import Data.Maybe ( fromMaybe )
import Control.Monad ( unless )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.DeepSeq ( NFData(..) )

import System.FilePath ( takeDirectory, (</>) )
import System.Directory ( createDirectoryIfMissing )
import System.Console.GetOpt ( OptDescr(Option), ArgDescr(ReqArg) )

import Data.Version ( showVersion )
import Paths_agda2rust ( version )

import Agda.Syntax.Common.Pretty ( prettyShow )
import Agda.Syntax.Internal ( qnameName, qnameModule )
import Agda.Syntax.TopLevelModuleName ( TopLevelModuleName, moduleNameToFileName )

import Agda.Compiler.Common ( curIF, compileDir )
import Agda.Compiler.Backend ( Backend(..), Backend'(..), Recompile(..), IsMain )

import Agda.TypeChecking.Monad.Base ( Definition(..) )
import Agda.TypeChecking.Monad
  ( TCM, withCurrentModule, iInsideScope, setScope
  , CompilerPragma(..), getUniqueCompilerPragma )

import Agda.Main ( runAgda )

import qualified Language.Rust.Pretty as R

import Agda2Rust ( convert, ignoreDef, report, ppm )

--

main = runAgda [Backend backend]

data Options = Options { optOutDir :: Maybe FilePath }

instance NFData Options where
  rnf _ = ()

outdirOpt :: Monad m => FilePath -> Options -> m Options
outdirOpt dir opts = return opts{ optOutDir = Just dir }

defaultOptions :: Options
defaultOptions = Options{ optOutDir = Nothing }

type ModuleEnv = ()
type ModuleRes = ()
type CompiledDef = String

backend :: Backend' Options Options ModuleEnv ModuleRes CompiledDef
backend = Backend'
  { backendName           = "agda2rust"
  , backendVersion        = Just (showVersion version)
  , options               = defaultOptions
  , commandLineFlags      =
      [ Option ['o'] ["out-dir"] (ReqArg outdirOpt "DIR")
        "Write output files to DIR. (default: project root)"
      ]
  , isEnabled             = \ _ -> True
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

compile :: Options -> ModuleEnv -> IsMain -> Definition -> TCM CompiledDef
compile opts tlm _ def@(Defn{..})
  | ignoreDef theDef
  = return ""
  | otherwise
  = withCurrentModule (qnameModule defName)
  -- $ getUniqueCompilerPragma "AGDA2RUST" defName >>= \case
  --     Nothing -> return []
  --     Just (CompilerPragma _ _) -> ...
  $ show . R.pretty' <$> convert def

writeModule :: Options -> ModuleEnv -> IsMain -> TopLevelModuleName
            -> [CompiledDef] -> TCM ModuleRes
writeModule opts _ _ m cdefs = do
  outDir <- compileDir
  let outFile = fromMaybe outDir (optOutDir opts) <> "/" <> moduleNameToFileName m "rs"
  let outS = "// *** module " <> prettyShow m <> " ***\n" <> unlines cdefs
  report outS
  unless (all null cdefs) $
    writeRsFile outFile outS
  where
    writeRsFile :: FilePath -> String -> TCM ()
    writeRsFile outFn content = liftIO $ do
      let outDir = takeDirectory outFn
      createDirectoryIfMissing True outDir
      writeFile outFn content
