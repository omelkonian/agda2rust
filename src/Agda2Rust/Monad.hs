module Agda2Rust.Monad where

import Control.Monad.Reader ( ReaderT(runReaderT), asks, local )
import Control.Monad.State ( StateT, runStateT, get, modify )

import qualified Data.Set as S ( Set, empty, insert, member )
import qualified Data.Map as M ( Map, empty, insert, lookup )

import Utils
import Agda
  ( TCM, liftTCM, QName, ConHead(..), ifM
  , unqual, vArgs, pp )
import Agda.Builtins ( isBuiltinDef )
import Agda2Rust.Pragma ( PragmaQualifier )

-- | TCM monad extended with a custom environment and state.
type C = StateT State (ReaderT Env TCM)

runC :: State -> C a -> TCM (a, State)
runC s0 k = runReaderT (runStateT k s0) initEnv

runC0 :: C a -> TCM (a, State)
runC0 = runC initState

-- | Environment tracking which part of a definition we are currently compiling.
data Env = Env
  { curDatatype    :: Maybe QName
  , curConstructor :: Maybe QName
  , curArgument    :: Int
  }

initEnv :: Env
initEnv = Env
  { curDatatype    = Nothing
  , curConstructor = Nothing
  , curArgument    = 0
  }

type QNameS   = String
type ConHeadS = String

-- | Compilation state.
data State = State
  { boxedConstructors  :: S.Set (QNameS, Int)
    -- ^ which constructors of a datatype to box (i.e. the recursive positions)
  , recordConstructors :: S.Set ConHeadS
    -- ^ keep track of record constructors to handle them differently
  , unusedTyParams     :: M.Map QNameS [String]
    -- ^ track the unused type parameters of a definition
    -- (will be put into a "phantom" variant/field to bypass the Rust checker)
  , ffi                :: M.Map QNameS (Maybe PragmaQualifier, String)
    -- ^ registers definitions to be compiled to FFI calls
  , consts             :: S.Set QNameS
    -- ^ registers functions that should be treated as constants (via `const` or `static`)
    -- e.g. referring to them with `x` instead of `x()`
  } deriving (Show, Read)

initState :: State
initState = State
  { boxedConstructors  = S.empty
  , recordConstructors = S.empty
  , unusedTyParams     = M.empty
  , ffi                = M.empty
  , consts             = S.empty
  }

inDatatype, inConstructor :: QName -> C a -> C a
inDatatype n = local $ \e -> e
  { curDatatype = Just n }
inConstructor n = local $ \e -> e
  { curConstructor = Just n }

inNonConstructor :: C a -> C a
inNonConstructor = local $ \e -> e
  { curConstructor = Nothing }

inArgument :: Int -> C a -> C a
inArgument n = local $ \e -> e
  { curArgument = n }

nextArgument :: Int -> C a -> C a
nextArgument n = local $ \e -> e
  { curArgument = 1 + curArgument e }

setBoxedConstructor :: (String, Int) -> C ()
setBoxedConstructor n = modify $ \s -> s
  { boxedConstructors = S.insert n (boxedConstructors s) }

setBox :: C ()
setBox = do
  Just cn <- asks curConstructor
  i <- asks curArgument
  -- report $ "* setting box " <> pp (cn, i)
  setBoxedConstructor (pp cn, i)

getBox :: (QName, Int) -> C Bool
getBox (cn, i) = S.member (pp cn, i) . boxedConstructors <$> get

shouldBox :: C Bool
shouldBox = asks curConstructor >>= \case
  Nothing -> return False
  Just cn -> do
    i <- asks curArgument
    -- report $ "* should box? " <> pp (cn, i)
    ret <- getBox (cn, i)
    -- report $ if ret then " yes!" else " no!"
    return ret

setRecordConstructor :: ConHead -> C ()
setRecordConstructor ConHead{..} = modify $ \s -> s
  { recordConstructors = S.insert (pp $ unqual conName) (recordConstructors s) }

isRecordConstructor :: QName -> C Bool
isRecordConstructor qn = S.member (pp $ unqual qn) . recordConstructors <$> get

setUnusedTyParams :: QName -> [String] -> C ()
setUnusedTyParams qn ps = modify $ \s -> s
  { unusedTyParams = M.insert (pp qn) ps (unusedTyParams s) }

hasUnusedTyParams :: QName -> C Bool
hasUnusedTyParams qn = ifM (liftTCM $ isBuiltinDef qn) (return False) $ do
  Just ps <- M.lookup (pp qn) . unusedTyParams <$> get
  return $ not (null ps)

setFFI :: QName -> (Maybe PragmaQualifier, String) -> C ()
setFFI qn v = modify \s -> s
  { ffi = M.insert (pp qn) v (ffi s) }

getFFI :: QName -> C (Maybe (Maybe PragmaQualifier, String))
getFFI qn = M.lookup (pp qn) . ffi <$> get

setConst :: QName -> C ()
setConst qn = modify \s -> s
  { consts = S.insert (pp qn) (consts s) }

isConst :: QName -> C Bool
isConst qn = S.member (pp qn) . consts <$> get
