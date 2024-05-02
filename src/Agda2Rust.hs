{-# LANGUAGE
  FlexibleInstances, FlexibleContexts
, ScopedTypeVariables
, OverloadedStrings
, DoAndIfThenElse
, BlockArguments
, MultiWayIf
#-}
module Agda2Rust where

import System.IO.Unsafe ( unsafePerformIO )

import Control.Monad.Error.Class ( MonadError(catchError) )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Monad ( unless, when, (<=<), forM, filterM )
import Control.Arrow ( first, second )
import Control.Monad.Reader ( ReaderT(runReaderT), asks, local )
import Control.Monad.State ( StateT, runStateT, get, gets, put, modify )

import Data.Generics ( Data, listify )

import Data.List ( elemIndex, partition, intercalate )
import Data.List.NonEmpty ( NonEmpty((:|)) )
import qualified Data.List.NonEmpty as NE ( fromList )
import Data.Maybe ( isJust, fromMaybe, mapMaybe )
import Data.Either ( isRight )
import Data.Word ( Word64, Word8 )
import qualified Data.Set as S ( Set, empty, insert, member )
import qualified Data.Map as M ( Map, empty, insert, lookup )
import qualified Data.Text as T ( pack, unpack )

-- * Bytes
import Data.Serializer ( toBytes )

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
  ( TTerm(..), TPrim(..), TAlt(..), EvaluationStrategy(..), isPrimEq
  , tLamView, mkTApp, mkTLam )
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
import qualified Agda.TypeChecking.Sort as A
  ( sortOf, sortOfType )
import qualified Agda.TypeChecking.Substitute as A
  ( TelV(..), raise )
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
-- * utils
import Agda.Utils.Monad ( ifM, mapMaybeM, anyM )
import Agda.Utils.Maybe ( ifJustM )
import Agda.Utils.List ( downFrom )
import Text.Show.Pretty ( ppShow )

-- * Rust
import qualified Language.Rust.Syntax as R
import qualified Language.Rust.Data.Ident as R
import qualified Language.Rust.Data.Position as R
import qualified Language.Rust.Parser as R
import qualified Language.Rust.Pretty as R

import Utils
import AgdaUtils
import RustUtils

-- | TCM monad extended with a custom environment.
data Env = Env
  { curDatatype    :: Maybe A.QName
  , curConstructor :: Maybe A.QName
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

data State = State
  { boxedConstructors  :: S.Set (QNameS, Int)
  , recordConstructors :: M.Map ConHeadS [String]
  , unusedTyParams     :: M.Map QNameS [String]
  } deriving (Show, Read)

initState :: State
initState = State
  { boxedConstructors  = S.empty
  , recordConstructors = M.empty
  , unusedTyParams     = M.empty
  }

type C = StateT State (ReaderT Env TCM)

runC :: State -> C a -> TCM (a, State)
runC s0 k = runReaderT (runStateT k s0) initEnv

runC0 :: C a -> TCM (a, State)
runC0 = runC initState

inDatatype, inConstructor :: A.QName -> C a -> C a
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
  report $ "* setting box " <> pp (cn, i)
  setBoxedConstructor (pp cn, i)

getBox :: (A.QName, Int) -> C Bool
getBox (cn, i) = do
  S.member (pp cn, i) . boxedConstructors <$> get

shouldBox :: C Bool
shouldBox = asks curConstructor >>= \case
  Nothing -> return False
  Just cn -> do
    i <- asks curArgument
    report $ "* should box? " <> pp (cn, i)
    ret <- getBox (cn, i)
    report $ if ret then " yes!" else " no!"
    return ret

setRecordConstructor :: A.ConHead -> C ()
setRecordConstructor A.ConHead{..} = modify $ \s -> s
  { recordConstructors = M.insert (pp $ unqual conName) (unqual <$> vArgs conFields) (recordConstructors s) }

isRecordConstructor :: A.QName -> C (Maybe [String])
isRecordConstructor qn = do
  M.lookup (pp $ unqual qn) . recordConstructors <$> get

setUnusedTyParams :: A.QName -> [String] -> C ()
setUnusedTyParams qn ps = modify $ \s -> s
  { unusedTyParams = M.insert (pp qn) ps (unusedTyParams s) }

hasUnusedTyParams :: A.QName -> C Bool
hasUnusedTyParams qn = ifM (liftTCM $ isBuiltinDef qn) (return False) $ do
  Just ps <- M.lookup (pp qn) . unusedTyParams <$> get
  return $ not (null ps)

-- | Converting between two types @a@ and @b@ under Agda's typechecking monad.
--
-- NB: 'go' is only used internally to de-clutter the recursive calls.
class (~>) a b | a -> b where
  convert, go :: a :~> b
  convert = go

type (:~>)  a b = a -> C (b ())
type (:~>*) a b = a -> C [b ()]

gos :: forall a b. a ~> b => [a] :~>* b
gos = igos . enumerate
  where
  igos :: [(Int, a)] :~>* b
  igos [] = return []
  igos ((i, a):as) = do -- (:) <$> inArgument i (go a) <$> igos as
    a' <- inArgument i (go a)
    as' <- igos as
    return (a' : as')

ignoreDef :: A.Definition -> TCM Bool
ignoreDef d@A.Defn{..} = do
  isBltin <- isBuiltinDef defName
  ignTy <- ignoreTy defType
  return $ case theDef of
    -- ** ignore builtin definitions
    _ | isBltin -> True
    -- ** ignore these types
    _ | ignTy -> True
    -- ** erasure
    _ | A.hasQuantity0 d -> True
    -- ** ignore functions that are @0/pattern-lambdas/with-generated
    A.Function{..} | {-funErasure ||-} isJust funExtLam || isJust funWith -> True
    -- ** ignored these kind of definitions
    A.Constructor{..} {-| conErasure-} -> True
    A.Primitive{..} -> True
    A.PrimitiveSort{..} -> True
    A.DataOrRecSig{..} -> True
    A.GeneralizableVar -> True
    -- ** compile everything else
    _ -> False
  where
  ignoreTy :: A.Type -> TCM Bool
  ignoreTy ty = do
    isLvl <- A.isLevelType =<< resTy ty
    return isLvl


-- | Compiling definitions.
instance A.Definition ~> R.Item where
  go A.Defn{..} = do
    report $ "*** compiling definition: " <> pp defName
    goD theDef
    where
    dx :: R.Ident
    dx | Just (recName, recField) <- isRecordProjection theDef
       = unqualField recName recField
       | otherwise
       = unqualR defName

    -- ** helper for functions and axioms
    goFn :: C (R.Expr ()) -> (A.ListTel, A.Type) -> C (R.Item ())
    goFn goBody acc = do
      (ps, as, resTy, body) <- goFn' acc
      let fn -- | null ps && null as = {-RConst-}RStatic dx resTy body
             | otherwise = RFn -- (if null as then RConstFn else RFn)
                              dx
                              (RTyParam . R.mkIdent <$> ps)
                              (RFnTy as resTy)
                              (RBlock body)
      -- report $ " fn: " <> ppR fn
      return fn
      where
      goFn' :: (A.ListTel, A.Type) -> C ([String], [R.Arg ()], R.Ty (), R.Expr ())
      goFn' (d:tel, resTy) = do
        -- report $ "_telArg: " <> pp d
        d' <- renameTelArg d
        report $ "_telArg': " <> pp d'
        (ps0, as0) <- goTelArg d'
        (ps, as, resTy, body) <- A.addContext d' (goFn' (tel, resTy))
        return $ (ps0 <> ps, as0 <> as, resTy, body)
      goFn' ([], resTy) = do
        resTy' <- go resTy
        body <- goBody
        return ([], [], resTy', body)

      goTelArg :: A.Dom (String, A.Type) -> C ([String], [R.Arg ()])
      goTelArg d@(A.unDom -> (x, ty)) = do
        -- let isSrt = isSortTy ty
        isSrt <- isSortResTy ty
        isLvl <- A.isLevelType ty
        let shouldDrop = isLvl || A.hasQuantity0 d
        if isSrt then
          return ([x], [])
        else do
          ty' <- go ty
          return ([], [RArg (R.mkIdent x) ty' | not shouldDrop])

      renameTelArg :: A.Dom (String, A.Type) -> C (A.Dom (String, A.Type))
      renameTelArg d@(unDom -> (x, ty))
        | "_" <- x
        = do x' <- freshVarInCtx
             return $ d {A.unDom = (x', ty)}
        | otherwise
        = return d

    goD :: A.Defn :~> R.Item
    goD = \case
      A.AbstractDefn defn -> goD defn

      -- ** postulates
      A.Axiom{..} -> do
        report $ " type: " <> pp defType
        (tel, resTy) <- telListView defType
        goFn (return $ rPanic "POSTULATE") (tel, resTy)

      -- ** type aliases
      A.Function{..}
        | isSortTy defType
        , [A.Clause{..}] <- funClauses
        , A.EmptyTel <- clauseTel
        , [] <- namedClausePats
        , Just t <- clauseBody
        -> RTyAlias dx <$> go (typeFromTerm t)

      -- -- ** record projections
      -- A.Function{..} | Right proj <- funProjection -> do
      --   report $ " record projection: " <> pp (A.projOrig proj)
      --   return undefined

      -- ** functions
      -- A.Function{..} | d ^. funInline
      A.Function{..} -> do
        report $ " type: " <> pp defType
        tdef <- liftTCM $ A.toTreeless defName
        report $ " tdef: " <> pp tdef
        let (tterm, introVars) = stripTopTLams tdef
        report $ " tterm: " <> pp tterm
        report $ " intros: " <> pp introVars
        (tel0, _) <- telListView defType
        intros <- calculateIntros introVars tel0
        (tel, resTy) <- telListViewUpTo intros defType
        -- report $ " >>tel: " <> pp tel
        -- report $ " >>resTy: " <> pp resTy
        -- let ctx | Right A.Projection{..} <- funProjection
        --         , Just recName <- projProper
        --         = RMod (unqualR recName)
        --         | otherwise
        --         = id
        -- ctx <$> goFn (go tterm) (tel, resTy)
        goFn (go tterm) (tel, resTy)
        where
        stripTopTLams :: A.TTerm -> (A.TTerm, Int)
        stripTopTLams = \case
          A.TLam t -> second (+ 1) (stripTopTLams t)
          t        -> (t, 0)

        calculateIntros :: Int -> A.ListTel -> C Int
        calculateIntros _ tel = return (length tel)
        -- calculateIntros = go 0
        --   where
        --   go n vIntros (d@(A.unDom -> (x, ty)) : tel) = do
        --     isSrt <- isSortResTy ty
        --     isLvl <- A.isLevelType ty
        --     let shouldDrop = isLvl || A.hasQuantity0 d
        --         isNotV = isSrt || shouldDrop
        --     if isNotV then
        --       A.addContext d $ go (n + 1) vIntros tel
        --     else if vIntros == 0 then
        --       return n
        --     else
        --       A.addContext d $ go (n + 1) (vIntros - 1) tel
        --   go n _ [] = return n

      -- ** datatypes
      A.Datatype{..} -> inDatatype defName $ do
        -- cs <- zip dataCons <$> traverse typeOfConst dataCons)
        cs <- concat <$> forM dataCons (\dc -> do
          cDef <- A.instantiateDef =<< getConstInfo dc
          return [ (dc, A.defType cDef) | hasQuantityNon0 cDef]
          )
        report $ " cs: " <> pp cs
        A.TelV tel _ <- A.telViewUpTo dataPars defType
        report $ " tel: " <> pp tel
        params <- fmap R.mkIdent <$> mapMaybeM shouldKeepTyParam (A.telToList tel)
        variants <- A.addContext tel (gos cs)
        let unusedParams = filter (unusedIn variants) params
        setUnusedTyParams defName (ppR <$> unusedParams)
        return $ REnum dx (RTyParam <$> params) (variants <>
          [ RVariant "_Impossible" [ RField $ phantomField unusedParams ]
          | not (null unusedParams)
          ])

      -- ** records
      A.Record{..} -> do
        -- NB: incorporate conHead/namedCon in the future for accuracy
        --     + to solve the issue with private (non-public) fields
        report $ " recPars: " <> pp recPars
        report $ " recTel: " <> pp recTel
        report $ " recConHead: " <> pp recConHead
        setRecordConstructor recConHead
        let (tel, fs) = splitAt recPars (A.telToList recTel)
        report $ " tel: " <> pp tel
        report $ " fs: " <> pp fs
        params <- fmap R.mkIdent <$> mapMaybeM shouldKeepTyParam tel
        report $ " params: " <> show params
        fields <- A.addContext tel (goFs $ unDom <$> filter hasQuantityNon0 fs)
        report $ " fields: " <> show fields
        let unusedParams = filter (unusedIn fields) params
        setUnusedTyParams defName (ppR <$> unusedParams)
        return $ RStruct dx (RTyParam <$> params) (fields <>
          [ RNamedField "_phantom" (phantomField unusedParams)
          | not (null unusedParams)
          ])


        where
        goFs :: [(String, A.Type)] :~>* R.StructField
        goFs [] = return []
        goFs ((x, ty):fs) =
          (:) <$> goF (x, ty)
              <*> A.addContext (A.defaultDom (x, ty)) (goFs fs)

        goF :: (String, A.Type) :~> R.StructField
        goF (x, ty) = RNamedField (R.mkIdent $ transcribe x) <$> go ty

      d -> panic "definition" d
     where
      phantomField :: [R.Ident] -> R.Ty ()
      phantomField ps
        = RPathTy
        $ RPath
          [ RPathSeg "std"
          , RPathSeg "marker"
          , RPathSeg' "PhantomData" (RAngles [R.TupTy (RTyRef <$> ps) ()])
          ]

-- | Compiling types.
instance A.Type ~> R.Ty where
  go ty = asks curDatatype >>= \curD -> do
    report $ " ** compiling type: " <> pp ty
    -- typeT <- liftTCM $ A.closedTermToTreeless A.LazyEvaluation (unEl ty)
    -- report $ " type (treeless): " <> pp typeT
    case unEl ty of
      -- ** defined types
      A.Def n es | es <- A.argsFromElims es ->
        ifJustM (isBuiltinTy n) (compileBuiltinTy es) $ do
          report $ " es: " <> pp es
          dTy <- A.typeOfConst n
          -- report $ " >> dTy: " <> pp dTy
          as <- flip filterM (enumerate0 es) \(i, a) -> do
            eTy <- getArgTy dTy i
            isLvl <- A.isLevelType eTy
            -- report $ " >> eTy: " <> pp eTy
            -- let isSrt = isSortTy eTy
            isSrt <- isSortResTy eTy
            return $ hasQuantityNon0 a && not isLvl && isSrt
          let toBox = curD == Just n
          when toBox setBox
          (if toBox then RBoxTy else id) . rTyRef' (unqualR n)
            <$> gos (typeFromTerm . unArg . snd <$> as)

      -- ** function types
      A.Pi a b ->
        if A.hasQuantity0 a then
          ctx (go $ A.unAbs b)
        else
          rBareFn (R.mkIdent x) <$> go (A.unDom a) <*> ctx (go $ A.unAbs b)
        where
          x   = A.bareNameWithDefault (A.absName b) (A.domName a)
          ctx = if isDependentArrow a then A.addContext [(x, a)] else id

      -- ** variables
      A.Var i es -> do
        x <- lookupCtxVar i
        -- es' <- traverse go (vArgs es)
        return $ RTyRef (R.mkIdent x)

      -- ** type lambdas
      -- TODO: currently only supports const lambdas `λ _ -> ...`
      A.Lam _ ty -> go (typeFromTerm $ A.unAbs ty)

      -- otherwise, error
      ty -> panic "type" ty
   where
    -- ** builtin types
    compileBuiltinTy :: [A.Arg A.Term] -> BuiltinTy -> C (R.Ty ())
    compileBuiltinTy as b = do
        unless (null as) $
          panic "primitive type (with non-null type parameters)" ty
        return $ RTyRef $ R.mkIdent $ case b of
          Nat    -> "i32"
          Float  -> "f64"
          Char   -> "char"
          String -> "String"
          Bool   -> "bool"
          Int    -> "i32"

-- | Compiling Agda constructors into Rust variants.
instance (A.QName, A.Type) ~> R.Variant where
  go (c, ty) = inConstructor c $ do
    report $ "** compiling constructor: " <> pp c
    as <- argTys ty
    -- as <- vargTys ty
    report $ " as: " <> pp as
    RVariant (unqualR c) <$> goFs 1 ({-unDom <$> filter shouldKeep-} as)
    where
      goFs :: Int -> [A.Dom (String, A.Type)] :~>* R.StructField
      goFs _ [] = return []
      -- goFs i ((x, ty):fs) =
      --   (:) <$> inArgument i (goF (x, ty))
      --       <*> A.addContext (A.defaultDom (x, ty)) (goFs (i + 1) fs)
      goFs i (a@(unDom -> (x, ty)):fs) =
        let ctx = A.addContext a in
        if shouldKeep a then
          (:) <$> inArgument i (goF (x, ty))
              <*> ctx (goFs (i + 1) fs)
        else
          ctx (goFs i fs)

      goF :: (String, A.Type) :~> R.StructField
      goF (x, ty) = RField <$> go ty

-- | Compiling (treeless) Agda terms into Rust expressions.
instance A.TTerm ~> R.Expr where
  go = boxTerm <=< (\case
    A.TVar i -> do
      -- iTy <- lookupCtxTy i
      report $ "* compiling variable: " <> pp i -- <> " : " <> pp iTy
      ctx <- currentCtx
      report $ " ctx: " <> pp ctx
      RExprRef . R.mkIdent <$> lookupCtxVar i
    A.TLit l -> rLit <$> go l
    t@(A.TDef _)  -> go (A.TApp t [])
    t@(A.TCon _)  -> go (A.TApp t [])
    t@(A.TPrim _) -> go (etaExpand 2 t)
      where
      etaExpand n t = A.mkTLam n $ A.raise n t `A.mkTApp` map A.TVar (downFrom n)
    A.TApp t (onlyNonErased -> ts) -> do -- goHead t <*> gos (onlyNonErased ts)
      report $ "* compiling application: " <> pp t <> " $ " <> pp ts
      (cn, h) <- goHead t
      (ts', ps) <- separateTyParams ts `catchError` \_ -> pure (ts, [])
      report $ "  ps: " <> pp ps
      report $ "  ts': " <> pp ts'
      ps' <- gos ps
      ts'' <- maybe inNonConstructor inConstructor cn $ gos ts'
      -- report $ "  ts'': " <> ppR ts''
      return $ h ps' ts''
    A.TLam t -> do
      let (n, b) = A.tLamView (A.TLam t)
      report $ " n: " <> pp n
      report $ " b: " <> pp b
      xs <- freshVarsInCtx n
      report $ " xs: " <> pp xs
      rLam (R.mkIdent <$> xs) <$> A.addContext (zip xs $ repeat defaultTy) (go b)
      -- panic "treeless term" (A.TLam t)
    A.TLet t t' -> do
      x <- freshVarInCtx
      e  <- go t
      e' <- A.addContext [(x, defaultTy)] $ go t'
      return $ rLet [(x, e)] e'
    t@(A.TCase scrutinee _ defCase alts) -> do
      report $ "* compiling case expression:\n" <> pp t
      ctx <- currentCtx
      report $ " ctx: " <> pp ctx
      report $ " scrutineeVar: " <> pp scrutinee
      (x, ty) <- lookupCtx scrutinee -- (pred (length ctx) - scrutinee)
      report $ " scrutinee: " <> x <> " : " <> pp ty
      arms <- traverse go =<< filterM shouldKeepAlt alts
      -- report $ " arms: " <> ppShow arms
      defArm <- case defCase of
        A.TError _ -> do
          report $ " scrutineeTy: " <> pp ty
          shouldCatchAll <- A.reduce (unEl ty) >>= \case
            A.Def qn _ -> (theDef <$> getConstInfo qn) >>= \case
              A.Datatype{} -> hasUnusedTyParams qn
              _ -> return False
          return [RArm RWildP (rPanic "IMPOSSIBLE") | shouldCatchAll]
        t -> do
          catchAll <- go t
          -- report $ " catchAll: " <> ppShow catchAll
          return [RArm RWildP catchAll]
      return $ RMatch (RExprRef $ R.mkIdent x) (arms <> defArm)

    -- A.TUnit ->
    -- A.TSort ->
    -- A.TErased ->

    A.TCoerce t -> go t
    A.TError err -> do
      -- msg <- go $ A.LitString (T.pack $ ppShow err)
      let msg = ppShow err
      return $ RMacroCall (RRef "panic") (RStrTok msg)
    t -> panic "treeless term" t)
    where
    shouldKeepAlt :: A.TAlt -> C Bool
    shouldKeepAlt = \case
      A.TACon{..} -> do
        cDef <- A.instantiateDef =<< getConstInfo aCon
        return $ hasQuantityNon0 cDef
      A.TAGuard{} -> pure True
      A.TALit{}   -> pure True

    boxTerm :: R.Expr () :~> R.Expr
    boxTerm e = do
      toBox <- shouldBox
      return $ (if toBox then RBox else id) e

    goHead :: A.TTerm -> C (Maybe A.QName, ([R.Expr ()] -> [R.Expr ()] -> R.Expr ()))
    goHead = \case
      A.TDef qn -> do
        rn <- getRid <$> getConstInfo qn
        -- toConst <- isNullary =<< A.typeOfConst qn
        -- return (Nothing, (if toConst then rCall else RCall) (unqualR qn))
        return (Nothing, \ps -> rCall' rn (rTyFromExp <$> ps))
        where
        getRid :: A.Definition -> R.Ident
        getRid A.Defn{..}
          | Just (recName, recField) <- isRecordProjection theDef
          = unqualField recName recField
          | otherwise
          = unqualR defName
      A.TCon cn -> do
        report $ "* compiling head of application (constructor: " <> pp cn <> ")"
        isRec <- A.isRecordConstructor cn
        report $ " isRec: " <> pp (isJust isRec)
        isBlt <- isBuiltinTerm cn
        if isJust isRec then do
          Right A.ConHead{..} <- A.getConHead cn
          h <- parentQualR cn
          let xs = unqualR . unArg <$> conFields
          return (Just cn , \_ es -> RMkStruct h (zipWith (\x e -> R.Field x (Just e) ()) xs es))
        else if isJust isBlt then do
          let Just bltE = isBlt
          return (Nothing, \[] [] -> compileBuiltinTerm bltE)
        else do
          h <- qualR cn
          return (Just cn, \_ es -> RCallCon (RPathExpr h) es)
      A.TPrim prim | Just binOp <- getBinOp prim ->
        return (Nothing, \_ [x, y] -> RBin binOp x y)
      A.TPrim A.PSeq ->
        return (Nothing, \_ -> last)
        -- | A.PIf <- prim
        -- , [] <- xs
      A.TVar n -> do
        -- f <- R.mkIdent <$> lookupCtxVar n
        -- return (Nothing, rCall f)
        (f, ty) <- lookupCtx n
        toConst <- isNullary ty
        return (Nothing, \_ -> (if toConst then rCall else RCall) (R.mkIdent f))
      hd -> panic "head" (ppShow hd)

    getBinOp :: A.TPrim -> Maybe R.BinOp
    getBinOp = \case
      A.PAdd   -> Just R.AddOp
      A.PAdd64 -> Just R.AddOp
      A.PSub   -> Just R.SubOp
      A.PSub64 -> Just R.SubOp
      A.PMul   -> Just R.MulOp
      A.PMul64 -> Just R.MulOp
      -- A.PQuot ->
      -- A.PQuot64 ->
      A.PRem   -> Just R.RemOp
      A.PRem64 -> Just R.RemOp
      A.PGeq   -> Just R.GeOp
      A.PLt    -> Just R.LtOp
      A.PLt64  -> Just R.LtOp
      prim | A.isPrimEq prim -> Just R.EqOp
      -- A.PEqI ->
      -- A.PEq64 ->
      -- A.PEqF ->
      -- A.PEqS ->
      -- A.PEqC ->
      -- A.PEqQ ->
      _ -> Nothing
      -- A.PIf ->
      -- A.PSeq ->
      -- A.PITo64 ->

-- | Compiling match clauses into case expressions.
instance A.TAlt ~> R.Arm where
  go = \case
    (A.TACon con n body) -> do
      report $ "* compiling arm (constructor): " <> pp con
      recCons <- recordConstructors <$> get
      report $ " recordConstructors: " <> pp recCons
      isRec <- isRecordConstructor con
      report $ " isRec: " <> pp isRec
      isBlt <- isBuiltinTerm con
      case isRec of
        -- compiling match on a record/struct value
        Just _ -> do
          path <- parentQualR con
          report $ " path: " <> ppR path
          (_, vas, _) <- viewTy =<< A.typeOfConst con
          vas' <- populateArgNames vas
          report $ " vas': " <> pp vas'
          let xs = transcribe . fst . unDom <$> filter hasQuantityNon0 (take n vas')
          Just (qn, _) <- A.isRecordConstructor con
          hasPhantomField <- hasUnusedTyParams qn
          let pats = RId . R.mkIdent <$> xs <> ["_phantom" | hasPhantomField]
          report $ " pat: " <> pp con <> "(" <> show n <> ")"
                <> " ~> " <> ppR path <> "(" <> intercalate "," (map ppR pats) <> ")"
          body'  <- A.addContext vas' (go body)
          report $ " body: " <> pp body <> " ~> " <> ppR body'
          return $ RArm (RStructP path (RNoFieldP <$> pats)) body'

        Nothing -> case isBlt of
          -- compiling match on builtin value
          Just bltE -> do
            body' <- go body
            return $ RArm (RLitP $ compileBuiltinTerm bltE) body'
          -- compiling match on a data/enum value
          Nothing -> do
            path <- qualR con
            (_, vas, _) <- viewTy =<< A.typeOfConst con
            vas' <- populateArgNames vas
            let xs = take n (fst . unDom <$> filter hasQuantityNon0 vas')
            let pats = RId . R.mkIdent <$> xs
            report $ " pat: " <> pp con <> "(" <> show n <> ")"
                  <> " ~> " <> ppR path <> "(" <> intercalate "," (map ppR pats) <> ")"
            body'  <- unboxPats con n xs =<< A.addContext vas' (go body)
            report $ " body: " <> pp body <> " ~> " <> ppR body'
            return $ RArm (RTupleP path pats) body'
      where
      populateArgNames :: A.ListTel -> C (A.ListTel)
      populateArgNames [] = return []
      populateArgNames (d@(A.unDom -> (x, ty)):tel)
        | "_" <- x
        = do x' <- freshVarInCtx
             let d' = d {A.unDom = (x', ty)}
             (d' :) <$> A.addContext d' (populateArgNames tel)
        | otherwise
        = (d :) <$> populateArgNames tel

      unboxPats :: A.QName -> Int -> [String] -> R.Expr () :~> R.Expr
      unboxPats con n xs e = inConstructor con $ do
        report $ "* unboxing " <> show xs
        ps <- flip mapMaybeM (enumerate xs) $ \(i, x) -> inArgument i $ do
          toBox <- shouldBox
          return $ if toBox then Just (x, RDeref (R.mkIdent x))
                            else Nothing
        return $ rLet ps e
    A.TAGuard guard body -> do
      report $ "* compiling arm (guard): " <> pp guard
      guard' <- go guard
      body'  <- go body
      return $ RGuardedArm RWildP guard' body'
    A.TALit lit body -> do
      report $ "* compiling arm (literal): " <> pp lit
      lit'  <- RLit <$> go lit
      body' <- go body
      return $ RArm (RLitP lit') body'

-- | Compiling literals.
instance A.Literal ~> R.Lit where
  go = return . \case
    A.LitNat    i -> R.Int R.Dec i R.Unsuffixed ()
    A.LitWord64 w -> R.ByteStr (toBytes w) R.Cooked R.Unsuffixed ()
    A.LitFloat  d -> R.Float d R.Unsuffixed ()
    A.LitString s -> R.Str (T.unpack s) R.Cooked R.Unsuffixed ()
    A.LitChar   c -> R.Char c R.Unsuffixed ()
    l -> panic "literal" l

unqualR :: A.QName -> R.Ident
unqualR = R.mkIdent . unqual

unqualField :: A.QName -> A.QName -> R.Ident
unqualField recName recField = unqualR recName <> "·" <> unqualR recField

-- unqualR' :: A.QName -> C R.Ident
-- unqualR' n = ifJustM (isBuiltinTerm n) (return . compileBuiltinTerm) $ do
--   R.mkIdent $ unqual n
--   where
compileBuiltinTerm :: BuiltinTerm -> R.Expr ()
compileBuiltinTerm = \case
  TrueE  -> RLitTrue
  FalseE -> RLitFalse

qualR :: A.QName :~> R.Path
qualR qn = do
  Right con <- fmap A.conName <$> A.getConHead qn
  ty  <- A.getConstructorData qn
  return $ RConRef (unqualR ty) (unqualR con)

qParent :: A.QName -> A.QName
qParent =
  A.qnameFromList . NE.fromList . reverse . drop 1 . reverse . A.qnameToList0

parentQualR :: A.QName :~> R.Path
parentQualR qn = do
  con <- A.getConstructorData qn
  return $ RRef (unqualR con)
