{-# LANGUAGE FlexibleContexts, OverloadedStrings, DoAndIfThenElse #-}

-- | Agda utilities.
module AgdaUtils where

import Control.Monad.Error.Class ( MonadError(throwError) )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Arrow ( first )

import Data.List ( partition )
import qualified Data.List.NonEmpty as NE ( fromList )
import Data.Maybe ( isJust )

-- * abstract syntax
import Agda.Syntax.Abstract.Name
  ( qnameToList0 )
-- * internal syntax
import Agda.Syntax.Common ( unArg )
import Agda.Syntax.Common
import Agda.Syntax.Internal
import Agda.Syntax.Literal
import Agda.Syntax.Internal
  ( QName, absName, qnameName, qnameModule, Abs(..), unAbs, unEl, unDom
  , nameId, conName, dbPatVarIndex, pDom, telToList, telFromList )
import Agda.Syntax.Translation.InternalToAbstract ( NamedClause(..) )
-- * treeless syntax
import Agda.Compiler.ToTreeless
  hiding ( toTreeless )
import AgdaInternals
  ( toTreeless )
import Agda.Syntax.Treeless
  ( TTerm(..), TPrim(..), TAlt(..), EvaluationStrategy(..), isPrimEq )
import Agda.Compiler.Treeless.Pretty ()
import Agda.Compiler.Treeless.EliminateLiteralPatterns
  ( eliminateLiteralPatterns )
-- * typechecking
import Agda.TypeChecking.Monad
import Agda.TypeChecking.Monad
  ( TCM, MonadTCM, liftTCM
  , theDef, defName
  , typeOfConst, getConstInfo
  , reportSLn, VerboseLevel )
import Agda.TypeChecking.Free ( freeVars, VarCounts(..) )
import Agda.TypeChecking.Datatypes
  ( getConstructorData, getConHead )
import Agda.TypeChecking.Records
  ( isRecord, isRecordConstructor, isRecordType )
import Agda.TypeChecking.Level
  ( isLevelType )
import Agda.TypeChecking.Substitute
  ( TelV(..) )
import Agda.TypeChecking.Telescope
  ( telViewPath, telViewUpTo, telView )
import Agda.TypeChecking.Primitive
  ( isBuiltin )
-- * reduction
import Agda.TypeChecking.Reduce
  ( reduce )
-- * pretty-printing
import qualified Agda.Syntax.Common.Pretty as P
  ( Pretty, prettyShow, renderStyle, Style(..), Mode(..) )
import Agda.TypeChecking.Pretty
  ( PrettyTCM(..), MonadPretty, fsep, punctuate, braces, parens, Doc )
import qualified Agda.TypeChecking.Pretty as P
  hiding (text)
import Text.Show.Pretty ( ppShow )

import Agda.Utils.Monad ( mapMaybeM, partitionM, ifM )
import Agda.Utils.Maybe ( boolToMaybe )

import Utils

-- ** pretty-printing
pp :: P.Pretty a => a -> String
pp = P.prettyShow

ppm :: (MonadPretty m, P.PrettyTCM a) => a -> m Doc
ppm = P.prettyTCM

prender :: Doc -> String
prender = P.renderStyle (P.Style P.OneLineMode 0 0.0)

pinterleave :: (Applicative m, Semigroup (m Doc)) => m Doc -> [m Doc] -> m Doc
pinterleave sep = fsep . punctuate sep

pbindings :: (MonadPretty m, PrettyTCM a) => [(String, a)] -> [m Doc]
pbindings = map $ \(n, ty) -> parens $ ppm n <> " : " <> ppm ty

panic :: (P.Pretty a, Show a) => String -> a -> b
panic s t = error $
  "[PANIC] unexpected " <> s <> ": " <> limit (pp t) <> "\n"
    <> "show: " <> limit (ppShow t)
  where limit = take 500

-- ** typechecking context
currentCtx :: MonadTCEnv m => m [(String, Type)]
currentCtx = fmap (first pp . unDom) <$> getContext

reportCurrentCtx :: (MonadIO m, MonadTCEnv m) => m ()
reportCurrentCtx = currentCtx >>= \ctx ->
  report $ "currentCtx: " <> pp ctx

lookupCtx :: MonadTCEnv m => Int -> m (String, Type)
lookupCtx i = first transcribe . (!! i) {-. reverse-} <$> currentCtx

currentCtxVars :: MonadTCEnv m => m [String]
currentCtxVars = fmap fst <$> currentCtx

lookupCtxVar :: MonadTCEnv m => Int -> m String
lookupCtxVar i = transcribe . (!! i) {-. reverse-} <$> currentCtxVars

currentCtxTys :: MonadTCEnv m => m [Type]
currentCtxTys = fmap snd <$> currentCtx

lookupCtxTy :: MonadTCEnv m => Int -> m Type
lookupCtxTy i = (!! i) {-. reverse-} <$> currentCtxTys

-- ** variables
unqual :: QName -> String
unqual = transcribe . pp . qnameName

qParent :: QName -> QName
qParent =
  qnameFromList . NE.fromList . reverse . drop 1 . reverse . qnameToList0

varPool :: [String]
varPool = zipWith (<>) (repeat "x") (show <$> [0..])

getVarPool :: MonadTCEnv m => m [String]
getVarPool = do
  xs <- currentCtxVars
  return $ filter (`elem` xs) varPool

freshVar :: [String] -> String
freshVar xs = head $ dropWhile (`elem` xs) varPool

freshVarInCtx :: MonadTCEnv m => m String
freshVarInCtx = freshVar <$> currentCtxVars

freshVarsInCtx :: (MonadTCEnv m, MonadAddContext m) => Int -> m [String]
freshVarsInCtx n | n < 0 = error $ "[freshVarsInCtx] negative number of variables"
freshVarsInCtx 0 = return []
freshVarsInCtx n = do
  x <- freshVar <$> currentCtxVars
  addContext [(x, defaultTy)] $
    (x :) <$> freshVarsInCtx (n - 1)

-- ** arguments & visibility

hasQuantityNon0 :: LensQuantity a => a -> Bool
hasQuantityNon0 = not . hasQuantity0

shouldKeep :: (LensQuantity a, LensHiding a) => a -> Bool
shouldKeep = visible /\ hasQuantityNon0

shouldKeepTyParam :: PureTCM m => Dom (ArgName, Type) -> m (Maybe ArgName)
shouldKeepTyParam d@(unDom -> (x, ty)) = do
  isSrt <- isSortResTy ty
  return $ boolToMaybe (hasQuantityNon0 d && isSrt) x

vArgs :: [Arg a] -> [a]
vArgs = fmap unArg . filter shouldKeep

vElims :: [Elim] -> [Term]
vElims = vArgs . argsFromElims

isLevelTerm, isSortTerm :: Term -> Bool
-- isTyParam = \case
--   Var _ _   -> True
--   Def _ _   -> True
--   Con _ _ _ -> True
--   Pi _ _    -> True
--   Sort _    -> False
--   _         -> False
isLevelTerm = \case
  Level _ -> True
  _       -> False
isSortTerm = isJust . isSort

isLevelTy, isSortTy :: Type -> Bool
isLevelTy = isLevelTerm . unEl
isSortTy  = isSortTerm  . unEl

isSortResTy :: PureTCM m => Type -> m Bool
isSortResTy ty = isSortTy <$> resTy ty

isSortM :: MonadTCEnv m => Term -> m Bool
isSortM = \case
  Sort _  -> return True
  Var n _ -> isSortTy <$> lookupCtxTy n
  _       -> return False

-- typeOf :: (MonadTCEnv m) => Term -> m Type
-- typeOf = \case
--   Var n _ -> typeOfBV n
--   Def n _ -> typeOfBV n

-- ** erasure
isErasedTTerm :: TTerm -> Bool
isErasedTTerm = \case
  TErased -> True
  TUnit   -> True
  TSort   -> True
  _       -> False

onlyNonErased :: [TTerm] -> [TTerm]
onlyNonErased = filter (not . isErasedTTerm)

isTyParam :: (ReadTCState m, HasConstInfo m, MonadTCEnv m) => TTerm -> m Bool
isTyParam = \case
  TDef n -> isSortTy <$> typeOfConst n
  TVar i -> isSortTy <$> lookupCtxTy i
  _      -> pure False

separateTyParams :: (ReadTCState m, HasConstInfo m, MonadTCEnv m) => [TTerm] -> m ([TTerm], [TTerm])
separateTyParams = partitionM isTyParam

-- ** types & telescopes
isDependentArrow :: Dom Type -> Bool
isDependentArrow ty = pp (domName ty) `notElem` ["_", "(nothing)"]

-- _undefined :: MonadError e m => m a
-- _undefined = throwError undefined

typeFromTerm :: a -> Type'' Term a
typeFromTerm = El (DummyS "???" :: Sort)

defaultTy :: Dom Type
defaultTy = defaultDom $ typeFromTerm (Dummy "???" [] :: Term)

telListView :: PureTCM m => Type -> m (ListTel, Type)
telListView t = do
  TelV tel t <- telView t -- telViewPath t
  return (telToList tel, t)

telListViewUpTo :: PureTCM m => Int -> Type -> m (ListTel, Type)
telListViewUpTo n t = do
  TelV tel t <- telViewUpTo n t
  return (telToList tel, t)

getArgTy :: PureTCM m => Type -> Int -> m Type
getArgTy ty i = do
  (tel, _) <- telListView ty
  return $ (snd . unDom <$> tel) !! i

viewTy :: PureTCM m => Type -> m (ListTel, ListTel, Type)
viewTy ty = do
  (tel, resTy) <- telListView ty
  -- let (vas, has) = partition shouldKeep tel
  let (vas, has) = partition visible tel
  return (has, vas, resTy)

argTys :: PureTCM m => Type -> m ListTel
argTys ty = fst <$> telListView ty

vargTys :: PureTCM m => Type -> m ListTel
vargTys ty = do
  (_ , vas, _) <- viewTy ty
  return vas

resTy :: PureTCM m => Type -> m Type
resTy ty = do
  (_ , _, ty) <- viewTy ty
  return ty

isNullary :: PureTCM m => Type -> m Bool
isNullary ty = null . filter hasQuantityNon0 . fst <$> telListView ty

-- filterTel :: (Dom Type -> Bool) -> Telescope -> Telescope
-- filterTel p = \case
--   EmptyTel -> EmptyTel
--   ExtendTel a tel
--     (if p a then ExtendTel
--     | ->
--     | otherwise -> traverseF (filterTel p) tel

-- ** definitions

isRecordProjection :: Defn -> Maybe (QName, QName)
isRecordProjection d
  | Function{..} <- d
  , Right Projection{..} <- funProjection
  , Just recName <- projProper
  = Just (recName, projOrig)
  | otherwise
  = Nothing

-- ** builtins
getBuiltins :: TCM [QName]
getBuiltins = mapMaybeM getBuiltinName' allBuiltinIds

data BuiltinTy = Nat | Float | Char | String | Bool | Int

isBuiltinDef :: QName -> TCM Bool
isBuiltinDef n = (n `elem`) <$> getBuiltins

isBuiltinTy :: (HasBuiltins m, MonadReduce m) => QName -> m (Maybe BuiltinTy)
isBuiltinTy n
  = builtinNat     .-> Nat
  $ builtinFloat   .-> Float
  $ builtinChar    .-> Char
  $ builtinString  .-> String
  $ builtinBool    .-> Bool
  $ builtinInteger .-> Int
  $ return Nothing
  where (.->) def ty = ifM (isBuiltin n def) (return $ Just ty)

data BuiltinTerm = TrueE | FalseE

isBuiltinTerm :: (HasBuiltins m, MonadReduce m) => QName -> m (Maybe BuiltinTerm)
isBuiltinTerm n
  = builtinTrue   .-> TrueE
  $ builtinFalse  .-> FalseE
  $ return Nothing
  where (.->) def ty = ifM (isBuiltin n def) (return $ Just ty)

allBuiltinIds :: [BuiltinId]
allBuiltinIds =
  builtinsNoDef <>
  [ builtinLevel,
  builtinNat,
  -- builtinSuc, builtinZero, builtinNatPlus, builtinNatMinus,
  -- builtinNatTimes, builtinNatDivSucAux, builtinNatModSucAux, builtinNatEquals,
  -- builtinNatLess,
  builtinInteger, -- builtinIntegerPos, builtinIntegerNegSuc,
  builtinWord64,
  builtinFloat, builtinChar, builtinString, builtinUnit, builtinUnitUnit,
  -- builtinSigma,
  builtinBool, builtinTrue, builtinFalse,
  -- builtinList, builtinNil, builtinCons, builtinIO,
  -- builtinMaybe, builtinNothing, builtinJust,
  -- builtinPath, builtinPathP,
  -- builtinItIsOne, builtinIsOne1, builtinIsOne2, builtinIsOneEmpty,
  -- builtinSubIn,
  -- builtinEquiv, builtinEquivFun, builtinEquivProof,
  -- builtinTranspProof,
  -- builtinInf, builtinSharp, builtinFlat,
  -- builtinEquality, builtinRefl, builtinRewrite, builtinLevelMax,
  -- builtinLevel, builtinLevelZero, builtinLevelSuc,
  -- builtinFromNat, builtinFromNeg, builtinFromString,
  -- builtinQName, builtinAgdaSort, builtinAgdaSortSet, builtinAgdaSortLit,
  -- builtinAgdaSortProp, builtinAgdaSortPropLit, builtinAgdaSortInf,
  -- builtinAgdaSortUnsupported,
  -- builtinHiding, builtinHidden, builtinInstance, builtinVisible,
  -- builtinRelevance, builtinRelevant, builtinIrrelevant,
  -- builtinQuantity, builtinQuantity0, builtinQuantityω,
  -- builtinModality, builtinModalityConstructor,
  -- builtinAssoc, builtinAssocLeft, builtinAssocRight, builtinAssocNon,
  -- builtinPrecedence, builtinPrecRelated, builtinPrecUnrelated,
  -- builtinFixity, builtinFixityFixity,
  -- builtinArgInfo, builtinArgArgInfo,
  -- builtinArg, builtinArgArg,
  -- builtinAbs, builtinAbsAbs, builtinAgdaTerm,
  -- builtinAgdaTermVar, builtinAgdaTermLam, builtinAgdaTermExtLam,
  -- builtinAgdaTermDef, builtinAgdaTermCon, builtinAgdaTermPi,
  -- builtinAgdaTermSort, builtinAgdaTermLit, builtinAgdaTermUnsupported, builtinAgdaTermMeta,
  -- builtinAgdaErrorPart, builtinAgdaErrorPartString, builtinAgdaErrorPartTerm, builtinAgdaErrorPartPatt, builtinAgdaErrorPartName,
  -- builtinAgdaLiteral, builtinAgdaLitNat, builtinAgdaLitWord64, builtinAgdaLitFloat,
  -- builtinAgdaLitChar, builtinAgdaLitString, builtinAgdaLitQName, builtinAgdaLitMeta,
  -- builtinAgdaClause, builtinAgdaClauseClause, builtinAgdaClauseAbsurd, builtinAgdaPattern,
  -- builtinAgdaPatVar, builtinAgdaPatCon, builtinAgdaPatDot, builtinAgdaPatLit,
  -- builtinAgdaPatProj, builtinAgdaPatAbsurd,
  -- builtinAgdaDefinitionFunDef,
  -- builtinAgdaDefinitionDataDef, builtinAgdaDefinitionRecordDef,
  -- builtinAgdaDefinitionDataConstructor, builtinAgdaDefinitionPostulate,
  -- builtinAgdaDefinitionPrimitive, builtinAgdaDefinition,
  -- builtinAgdaMeta,
  builtinAgdaTCM, builtinAgdaTCMReturn, builtinAgdaTCMBind, builtinAgdaTCMUnify,
  builtinAgdaTCMTypeError, builtinAgdaTCMInferType,
  builtinAgdaTCMCheckType, builtinAgdaTCMNormalise, builtinAgdaTCMReduce,
  builtinAgdaTCMCatchError,
  builtinAgdaTCMGetContext, builtinAgdaTCMExtendContext, builtinAgdaTCMInContext,
  builtinAgdaTCMFreshName, builtinAgdaTCMDeclareDef, builtinAgdaTCMDeclarePostulate, builtinAgdaTCMDeclareData, builtinAgdaTCMDefineData, builtinAgdaTCMDefineFun,
  builtinAgdaTCMGetType, builtinAgdaTCMGetDefinition,
  builtinAgdaTCMQuoteTerm, builtinAgdaTCMUnquoteTerm, builtinAgdaTCMQuoteOmegaTerm,
  builtinAgdaTCMCommit, builtinAgdaTCMIsMacro, builtinAgdaTCMBlock,
  builtinAgdaBlocker, builtinAgdaBlockerAll, builtinAgdaBlockerAny, builtinAgdaBlockerMeta,
  builtinAgdaTCMFormatErrorParts, builtinAgdaTCMDebugPrint,
  builtinAgdaTCMWithNormalisation, builtinAgdaTCMWithReconstructed,
  builtinAgdaTCMWithExpandLast, builtinAgdaTCMWithReduceDefs,
  builtinAgdaTCMAskNormalisation, builtinAgdaTCMAskReconstructed,
  builtinAgdaTCMAskExpandLast, builtinAgdaTCMAskReduceDefs,
  builtinAgdaTCMNoConstraints,
  builtinAgdaTCMRunSpeculative,
  builtinAgdaTCMExec,
  builtinAgdaTCMGetInstances,
  builtinAgdaTCMPragmaForeign,
  builtinAgdaTCMPragmaCompile
  ]



