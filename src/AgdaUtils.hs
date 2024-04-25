{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

-- | Agda utilities.
module AgdaUtils where

import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Arrow ( first )

import Data.List ( partition )
import qualified Data.List.NonEmpty as NE ( fromList )

-- * abstract syntax
import qualified Agda.Syntax.Abstract.Name as A
  ( qnameToList0 )
-- * internal syntax
import Agda.Syntax.Common ( unArg )
import qualified Agda.Syntax.Common as A
import qualified Agda.Syntax.Internal as A
import qualified Agda.Syntax.Literal as A
import Agda.Syntax.Internal
  ( QName, absName, qnameName, qnameModule, Abs(..), unAbs, unEl, unDom
  , nameId, conName, dbPatVarIndex, pDom, telToList, telFromList )
import Agda.Syntax.Translation.InternalToAbstract ( NamedClause(..) )
-- * treeless syntax
import qualified Agda.Compiler.ToTreeless as A
  hiding ( toTreeless )
import qualified AgdaInternals as A
  ( toTreeless )
import qualified Agda.Syntax.Treeless as A
  ( TTerm(..), TPrim(..), TAlt(..), EvaluationStrategy(..), isPrimEq )
import Agda.Compiler.Treeless.Pretty ()
import qualified Agda.Compiler.Treeless.EliminateLiteralPatterns as A
  ( eliminateLiteralPatterns )
-- * typechecking
import qualified Agda.TypeChecking.Monad as A
import Agda.TypeChecking.Monad
  ( TCM, MonadTCM, liftTCM
  , theDef, defName
  , typeOfConst, getConstInfo
  , reportSLn, VerboseLevel )
import Agda.TypeChecking.Free ( freeVars, VarCounts(..) )
import qualified Agda.TypeChecking.Datatypes as A
  ( getConstructorData, getConHead )
import qualified Agda.TypeChecking.Records as A
  ( isRecord, isRecordConstructor, isRecordType )
import qualified Agda.TypeChecking.Level as A
  ( isLevelType )
import qualified Agda.TypeChecking.Substitute as A
  ( TelV(..) )
import qualified Agda.TypeChecking.Telescope as A
  ( telViewPath, telViewUpTo )
-- * reduction
import qualified Agda.TypeChecking.Reduce as A
  ( reduce )
-- * pretty-printing
import Agda.Syntax.Common.Pretty as P
  ( Pretty, prettyShow, renderStyle, Style(..), Mode(..) )
import Agda.TypeChecking.Pretty
  ( PrettyTCM(..), MonadPretty, fsep, punctuate, braces, parens, Doc )
import qualified Agda.TypeChecking.Pretty as P
  hiding (text)
import Text.Show.Pretty ( ppShow )

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
currentCtx :: A.MonadTCEnv m => m [(String, A.Type)]
currentCtx = fmap (first pp . unDom) <$> A.getContext

reportCurrentCtx :: (MonadIO m, A.MonadTCEnv m) => m ()
reportCurrentCtx = currentCtx >>= \ctx ->
  report $ "currentCtx: " <> pp ctx

lookupCtx :: A.MonadTCEnv m => Int -> m (String, A.Type)
lookupCtx i = first transcribe . (!! i) <$> currentCtx

currentCtxVars :: A.MonadTCEnv m => m [String]
currentCtxVars = fmap fst <$> currentCtx

lookupCtxVar :: A.MonadTCEnv m => Int -> m String
lookupCtxVar i = transcribe . (!! i) <$> currentCtxVars

-- ** variables
unqual :: A.QName -> String
unqual = transcribe . pp . qnameName

qParent :: A.QName -> A.QName
qParent =
  A.qnameFromList . NE.fromList . reverse . drop 1 . reverse . A.qnameToList0

varPool :: [String]
varPool = zipWith (<>) (repeat "x") (show <$> [0..])

getVarPool :: A.MonadTCEnv m => m [String]
getVarPool = do
  xs <- currentCtxVars
  return $ filter (`elem` xs) varPool

freshVar :: [String] -> String
freshVar xs = head $ dropWhile (`elem` xs) varPool

freshVarInCtx :: A.MonadTCEnv m => m String
freshVarInCtx = freshVar <$> currentCtxVars

-- ** arguments & visibility

shouldKeep :: (A.LensQuantity a, A.LensHiding a) => a -> Bool
shouldKeep = A.visible /\ (not . A.hasQuantity0)

vArgs :: [A.Arg a] -> [a]
vArgs = fmap unArg . filter shouldKeep

vElims :: [A.Elim] -> [A.Term]
vElims = vArgs . A.argsFromElims

-- ** erasure
isErased :: A.TTerm -> Bool
isErased = \case
  A.TErased -> True
  _         -> False

onlyNonErased :: [A.TTerm] -> [A.TTerm]
onlyNonErased = filter (not . isErased)

-- ** types & telescopes
isDependentArrow :: A.Dom A.Type -> Bool
isDependentArrow ty = pp (A.domName ty) `notElem` ["_", "(nothing)"]

telListView :: A.PureTCM m => A.Type -> m (A.ListTel, A.Type)
telListView t = do
  A.TelV tel t <- A.telViewPath t
  return (A.telToList tel, t)

viewTy :: A.PureTCM m => A.Type -> m (A.ListTel, A.ListTel, A.Type)
viewTy ty = do
  (tel, resTy) <- telListView ty
  -- let (vargs, hargs) = partition shouldKeep tel
  let (vargs, hargs) = partition A.visible tel
  return (hargs, vargs, resTy)

vargTys :: A.PureTCM m => A.Type -> m A.ListTel
vargTys ty = do
  (_ , vargs, _) <- viewTy ty
  return vargs

resTy :: A.PureTCM m => A.Type -> m A.Type
resTy ty = do
  (_ , _, ty) <- viewTy ty
  return ty

-- filterTel :: (Dom Type -> Bool) -> A.Telescope -> A.Telescope
-- filterTel p = \case
--   A.EmptyTel -> A.EmptyTel
--   A.ExtendTel a tel
--     (if p a then A.ExtendTel
--     | ->
--     | otherwise -> traverseF (filterTel p) tel


