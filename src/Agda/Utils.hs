{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

-- | Agda utilities.
module Agda.Utils where

import Control.Monad.IO.Class ( MonadIO )
import Control.Monad ( filterM )
import Control.Arrow ( first )

import Data.List ( partition )
import qualified Data.List.NonEmpty as NE ( fromList )
import Data.Maybe ( isJust, isNothing )

import Utils
import Agda.Lib

-- ** useful monad constraint kinds

type MonadIOEnv m = (MonadIO m, MonadTCEnv m)
type PureEnvTCM m = (PureTCM m, ReadTCState m, HasConstInfo m, MonadIOEnv m)

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
reportCurrentCtx = do
  ctx <- currentCtx
  report $ " currentCtx: " <> pp ctx

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

varPool :: String -> [String]
varPool varName = zipWith (<>) (repeat varName) $ "" : (show <$> [0..])

freshVarInCtx :: MonadIOEnv m => String -> m String
freshVarInCtx varName = freshVar varName' <$> currentCtxVars
  where
  varName' = if varName == "_" then "x" else varName

  freshVar :: String -> [String] -> String
  freshVar varName xs = head $ dropWhile (`elem` xs) (varPool varName)

freshVarsInCtx :: (MonadIOEnv m, MonadAddContext m) => Int -> m [String]
freshVarsInCtx n | n < 0 = error $ "[freshVarsInCtx] negative number of variables"
freshVarsInCtx 0 = return []
freshVarsInCtx n = do
  x <- freshVarInCtx "_"
  addContext [(x, defaultTy)] $
    (x :) <$> freshVarsInCtx (n - 1)

-- ** arguments & visibility

type TelItem = Dom (ArgName, Type)
type Tel     = [TelItem]

unElims :: [Elim] -> [Term]
unElims = fmap unArg . argsFromElims

hasQuantityNon0 :: LensQuantity a => a -> Bool
hasQuantityNon0 = not . hasQuantity0

shouldKeepTyParam :: PureTCM m => TelItem -> m (Maybe ArgName)
shouldKeepTyParam d@(unDom -> (x, ty)) = do
  isSrt <- isSortResTy ty
  return $ boolToMaybe (hasQuantityNon0 d && isSrt) x

shouldKeepTel :: PureTCM m => ListTel -> m ListTel
shouldKeepTel = filterM (fmap isNothing . shouldKeepTyParam)

shouldKeep :: (LensQuantity a, LensHiding a) => a -> Bool
shouldKeep = visible /\ hasQuantityNon0

-- shouldKeepArgs :: [Arg a] -> [a]
-- shouldKeepArgs = fmap unArg . filter shouldKeep

-- shouldKeepElims :: [Elim] -> [Term]
-- shouldKeepElims = shouldKeepArgs . argsFromElims

isLevelTerm, isSortTerm :: Term -> Bool
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

isSrtOrLvlTy :: PureTCM m => Type -> m Bool
isSrtOrLvlTy ty = (||) <$> isSortResTy ty <*> isLevelType ty

isSortM :: MonadTCEnv m => Term -> m Bool
isSortM = \case
  Sort _  -> return True
  Var n _ -> isSortTy <$> lookupCtxTy n
  _       -> return False

-- typeOf :: (MonadTCEnv m) => Term -> m Type
-- typeOf = \case
--   Var n _ -> typeOfBV n
--   Def n _ -> typeOfBV n

erasedArity :: Type -> Int
erasedArity t = case unEl t of
  Pi a b -> (if hasQuantity0 a then 0 else 1) + erasedArity (unAbs b)
  _      -> 0

-- ** treeless terms

defNameOfT :: TTerm -> Maybe QName
defNameOfT = \case
  TDef n -> Just n
  TCon n -> Just n
  -- TPrim n -> ??
  -- TVar n -> ??
  _ -> Nothing

-- | Perform multiple η-expansions on a treeless term.
etaExpandT :: Int -> Int -> Int -> TTerm -> TTerm
etaExpandT n k l t
  = mkTLam nk
  $ raise nk t `mkTApp` map (raise l . TVar) (downFrom n)
  where nk = max (n - k) 0 -- safeguard

-- ** erasure

data ClassifiedArg
  = TyParam ArgName
  | LvlParam
  | ErasedArg
  | KeptArg ArgName Type

isKeptArg, isTyParam :: ClassifiedArg -> Bool
isKeptArg = \case {KeptArg{} -> True; _ -> False}
isTyParam = \case {TyParam{} -> True; _ -> False}

classifyArg :: PureTCM m => TelItem -> m ClassifiedArg
classifyArg d@(unDom -> (x, ty)) = do
  isSrt <- isSortResTy ty
  isLvl <- isLevelType ty
  return $ if
    | isSrt -> TyParam x
    | isLvl -> LvlParam
    | hasQuantity0 d -> ErasedArg
    | otherwise -> KeptArg x ty

classifyArgs :: PureTCM m => [TelItem] -> m [ClassifiedArg]
classifyArgs = mapM classifyArg

isErasedTTerm :: TTerm -> Bool
isErasedTTerm = \case
  TErased -> True
  TUnit   -> True
  TSort   -> True
  _       -> False

onlyNonErased :: [TTerm] -> [TTerm]
onlyNonErased = filter (not . isErasedTTerm)

isTyParamM :: PureEnvTCM m => TTerm -> m Bool
isTyParamM = \case
  TDef n -> isSortTy <$> typeOfConst n
  TVar i -> isSortTy <$> lookupCtxTy i
  TApp h _ -> do
    -- report $ "  t (head): " <> pp h
    isResTyParam h
    -- return False
    -- let ty = termFromTTerm tt
    -- resTy t
    where
      isResTyParam = \case
        TDef n -> isSortTy <$> (resTy =<< typeOfConst n)
        TVar i -> isSortTy <$> (resTy =<< lookupCtxTy i)
        _ -> pure False
        -- t -> panic "result type parameter" t
  _ -> return False

separateTyParams :: PureEnvTCM m  => [TTerm] -> m ([TTerm], [TTerm])
separateTyParams = partitionM isTyParamM

-- ** types & telescopes
isDependentArrow :: Dom Type -> Bool
isDependentArrow ty = pp (domName ty) `notElem` ["_", "(nothing)"]

typeFromTerm :: a -> Type'' Term a
typeFromTerm = El (DummyS "???" :: Sort)

elimFromTTerm :: TTerm -> Elim
elimFromTTerm = elimFromTerm . termFromTTerm

elimFromTerm :: Term -> Elim
elimFromTerm = Apply . defaultArg

termFromTTerm :: TTerm -> Term
termFromTTerm = \case
  TVar n -> Var n []
  TDef qn -> Def qn []
  TLit lit -> Lit lit
  -- TCon qn ->
  TApp t as -> let as' = elimFromTTerm <$> as in
    case termFromTTerm t of
      Var n [] -> Var n as'
      Def qn [] -> Def qn as'
      _ -> panic "treeless head" t
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
  -- (has, vas) <- partitionM (fmap isKeptArg . classifyArg) tel
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

isDataDef, isRecDef, isFunDef :: Defn -> Bool
isDataDef = \case
  Datatype{} -> True
  _ -> False

isRecDef = \case
  Record{} -> True
  _ -> False

isFunDef = \case
  Function{} -> True
  _ -> False

isRecordProjection :: Defn -> Maybe (QName, QName)
isRecordProjection d
  | Function{..} <- d
  , Right Projection{..} <- funProjection
  , Just recName <- projProper
  = Just (recName, projOrig)
  | otherwise
  = Nothing

funCC :: (MonadTCM m, HasConstInfo m) => QName -> m CompiledClauses
funCC q = do
  def <- theDef <$> getConstInfo q
  case def of
    Function{..} -> do
      case funCompiled of
        Just cc -> return cc
        Nothing -> panic "function clauses (not compiled yet)" def
    _ -> panic "definition (not a function)" def
