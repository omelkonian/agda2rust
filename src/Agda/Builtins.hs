module Agda.Builtins
  ( module Agda.Syntax.Builtin
  , isBuiltinDef
  , getBuiltins
  , allBuiltinIds
  )
  where

import Agda.Lib
import Agda.Syntax.Builtin

isBuiltinDef :: QName -> TCM Bool
isBuiltinDef n = (n `elem`) <$> getBuiltins

getBuiltins :: TCM [QName]
getBuiltins = mapMaybeM getBuiltinName' allBuiltinIds

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





