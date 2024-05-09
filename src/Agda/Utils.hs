{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

-- | Agda utilities.
module Agda.Utils where

import Control.Monad.IO.Class ( MonadIO )
import Control.Arrow ( first )

import Data.List ( partition )
import qualified Data.List.NonEmpty as NE ( fromList )
import Data.Maybe ( isJust )

import Utils
import Agda.Lib

-- ** pretty-printing
pp :: Pretty a => a -> String
pp = prettyShow

ppm :: (MonadPretty m, PrettyTCM a) => a -> m Doc
ppm = prettyTCM

prender :: Doc -> String
prender = renderStyle (Style OneLineMode 0 0.0)

pinterleave :: (Applicative m, Semigroup (m Doc)) => m Doc -> [m Doc] -> m Doc
pinterleave sep = fsep . punctuate sep

pbindings :: (MonadPretty m, PrettyTCM a) => [(String, a)] -> [m Doc]
pbindings = map $ \(n, ty) -> parens $ ppm n <> " : " <> ppm ty

panic :: (Pretty a, Show a) => String -> a -> b
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

returnTy :: Type -> Term
returnTy = flip (.) unEl $ \case
  Pi _ ty -> returnTy (unAbs ty)
  ty -> ty

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

typeFromTerm :: a -> Type'' Term a
typeFromTerm = El (DummyS "???" :: Sort)

termFromTTerm :: TTerm -> Term
termFromTTerm = \case
  TVar n -> Var n []
  TDef qn -> Def qn []
  TLit lit -> Lit lit
  -- TCon qn ->
  -- TApp tterm as -> Def qn
  t -> panic "tterm (to convert to term)" t

typeFromTTerm :: TTerm -> Type
typeFromTTerm = typeFromTerm . termFromTTerm

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


