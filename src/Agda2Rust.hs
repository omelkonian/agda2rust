{-# LANGUAGE
  FlexibleInstances, FlexibleContexts
, ScopedTypeVariables
, OverloadedStrings
, DoAndIfThenElse
#-}
-- | Conversion from Agda's internal syntax to our simplified JSON format.
module Agda2Rust where

import System.IO.Unsafe ( unsafePerformIO )

import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Monad ( unless, when, (<=<) )
import Control.Arrow ( first )
import Control.Monad.Reader ( ReaderT(runReaderT), asks, local )
import Control.Monad.State ( StateT, runStateT, get, gets, put, modify )

import Data.List ( elemIndex, partition, intercalate )
import Data.List.NonEmpty ( NonEmpty((:|)) )
import qualified Data.List.NonEmpty as NE ( fromList )
import Data.Maybe ( isJust, fromMaybe, mapMaybe )
import Data.Either ( isRight )
import Data.Word ( Word64, Word8 )
import qualified Data.Set as S ( Set, empty, insert, member )
import qualified Data.Map as M ( Map, empty, insert, lookup )
import qualified Data.Text as T ( pack, unpack )
import Data.Serializer ( toBytes )
import Unicode.Char.Identifiers ( isXIDStart, isXIDContinue )
import Data.Char ( ord )

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
-- * pretty-printing
import Agda.Syntax.Common.Pretty as P
  ( Pretty, prettyShow, renderStyle, Style(..), Mode(..) )
import Agda.TypeChecking.Pretty
  ( PrettyTCM(..), MonadPretty, fsep, punctuate, braces, parens, Doc )
import qualified Agda.TypeChecking.Pretty as P
  hiding (text)
-- * utils
import Agda.Utils.Monad ( ifM, mapMaybeM )

-- * Rust
import qualified Language.Rust.Syntax as R
import qualified Language.Rust.Data.Ident as R
import qualified Language.Rust.Data.Position as R
import qualified Language.Rust.Parser as R
import qualified Language.Rust.Pretty as R
import Text.Show.Pretty ( ppShow )

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

data State = State
  { boxedConstructors :: S.Set (String, Int)
  , recordConstructors :: M.Map String [String]
  } deriving (Show, Read)
initState :: State
initState = State
  { boxedConstructors = S.empty
  , recordConstructors = M.empty
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

ignoreDef :: A.Defn -> Bool
ignoreDef = \case
  A.Primitive{..} -> True
  A.PrimitiveSort{..} -> True
  A.Axiom{..} -> True
  A.DataOrRecSig{..} -> True
  A.GeneralizableVar -> True
  -- TODO
  A.Constructor{..} -> True
  _ -> False

type FnAccum = ([String], [R.Arg ()], R.Ty (), R.Expr ())

-- | Compiling definitions.
instance A.Definition ~> R.Item where
  go A.Defn{..} = do
    report $ "*** compiling definition: " <> pp defName
    goD theDef
    where
    dx = unqualR defName

    goD :: A.Defn :~> R.Item
    goD = \case
      A.AbstractDefn defn -> goD defn

      -- A.Function{..} | d ^. funInline
      def@(A.Function{..}) -> do
        report $ " type: " <> pp defType
        (tel, resTy) <- telListView defType
        tterm <- liftTCM $ A.toTreeless defName
        report $ " tterm: " <> pp tterm
        (tyParams, args, resTy, body) <- goFn (tel, resTy, tterm)
        report $ " fnTy: " <> ppR (RFnTy args resTy)
        let fn = RFn dx (RTyParam . R.mkIdent <$> tyParams)
                        (RFnTy args resTy)
                        (RBlock body)
        report $ " fn: " <> ppR fn
        return fn
        where
        goA :: A.Dom (String, A.Type) -> C ([String], [R.Arg ()])
        goA d@(A.unDom -> (x, ty))
          | isJust $ A.isSort $ A.unEl ty
          = return ([x], [])
          | otherwise
          = do isLvl <- A.isLevelType ty
               ty' <- go ty
               return ([], [RArg (R.mkIdent x) ty' | not isLvl])

        goFn :: (A.ListTel, A.Type, A.TTerm) -> C FnAccum
        goFn (d:tel, resTy, body) = do
          d' <- renameArg d
          (ps0, as0) <- goA d'
          (ps, as, resTy, body) <- A.addContext d' (goFn (tel, resTy, body))
          return $ (ps0 <> ps, as0 <> as, resTy, body)
        goFn ([], resTy, body) = do
          resTy' <- go resTy
          body' <- go (stripTopTLams body)
          return ([], [], resTy', body')

        renameArg :: A.Dom (String, A.Type) -> C (A.Dom (String, A.Type))
        renameArg d@(unDom -> (x, ty))
          | "_" <- x
          = do x' <- freshVarInCtx
               return $ d {A.unDom = (x', ty)}
          | otherwise
          = return d

        stripTopTLams :: A.TTerm -> A.TTerm
        stripTopTLams = \case
          A.TLam t -> stripTopTLams t
          t -> t

      A.Datatype{..} -> inDatatype defName $ do
        cs <- zip dataCons <$> traverse typeOfConst dataCons
        A.TelV tel _ <- A.telViewUpTo dataPars defType
        report $ "tel: " <> pp tel
        params   <- extractTyParams tel
        variants <- A.addContext tel (gos cs)
        return $ REnum dx (RTyParam <$> params) (variants <>
            [RVariant "_Impossible"
              [ RField
              $ RPathTy
              $ R.Path False
                [ RPathSegment "std"
                , RPathSegment "marker"
                , R.PathSegment "PhantomData" (Just $
                    R.AngleBracketed [] [R.TupTy (RTyRef <$> params) ()] [] ()
                  ) ()
                ] ()
              ]
            ])
        where
        extractTyParams :: A.Telescope -> C [R.Ident]
        extractTyParams tel = do
          let xs = mapMaybe dropTyParam (A.telToList tel)
          -- let xs = A.namedTelVars 0 tel
          -- xs <- traverse goA =<< fmap unDom <$> vargTys ty
          report $ "xs: " <> pp xs
          return (R.mkIdent <$> xs)
          where
            dropTyParam :: A.Dom (String, A.Type) -> Maybe String
            dropTyParam dst
              | (s, ty) <- unDom dst
              , Just _ <- A.isSort (unEl ty)
              = Just s
              | otherwise
              = Nothing
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
        let params = extractTyParams tel
        report $ " params: " <> show params
        fields <- A.addContext tel (goFs $ unDom <$> fs)
        report $ " fields: " <> show fields
        return $ RStruct dx (RTyParam <$> params) fields

        where
        extractTyParams :: A.ListTel -> [R.Ident]
        extractTyParams = map (R.mkIdent . fst . A.unDom)

        goFs :: [(String, A.Type)] :~>* R.StructField
        goFs [] = return []
        goFs ((x, ty):fs) =
          (:) <$> goF (x, ty)
              <*> A.addContext (A.defaultDom (x, ty)) (goFs fs)

        goF :: (String, A.Type) :~> R.StructField
        goF (x, ty) = RNamedField (R.mkIdent $ transcribe x) <$> go ty

    -- A.Constructor{..} -> do
    --   let cn = conName conSrcCon
    --   d <- theDef <$> getConstInfo conData
    --   return $ case d of
    --     A.Datatype{..} ->
    --       let Just i x= elemIndex (unqual cn) (unqual <$> dataCons)
    --       in  Constructor (pp conData) (toInteger ix)
    --     A.Record{..} -> Constructor (pp conData) 0
      d -> panic "unsupported definition" d

-- | Compiling types.
instance A.Type ~> R.Ty where
  go ty = asks curDatatype >>= \curD -> case unEl ty of
    A.Def n as
      | Just primTy <- R.mkIdent <$> goPrim n
      -> do
        unless (null as) $ panic "primitive types cannot have type parameters" ty
        return $ RTyRef primTy
      where
      goPrim :: QName -> Maybe String
      goPrim n = case pp n of
        "Agda.Builtin.Nat.Nat" -> Just "i32"
        _ -> Nothing
    A.Def n es | as <- vElims es -> do
      let toBox = curD == Just n
      when toBox setBox
      (if toBox then rBoxTy else id) . RTyRef' (unqualR n)
        <$> gos (A.El (undefined :: A.Sort) <$> as)

    A.Pi a b ->
      RBareFn (R.mkIdent x) <$> go (A.unDom a) <*> ctx (go $ A.unAbs b)
      where
        x   = A.bareNameWithDefault (A.absName b) (A.domName a)
        ctx = if isDependentArrow a then A.addContext [(x, a)] else id

    -- otherwise, error
    A.Var i es -> do
      x <- lookupCtxVar i
      -- es' <- traverse go (vArgs es)
      return $ RTyRef (R.mkIdent x)
    ty -> panic "unsupported type" ty

-- | Compiling a collection of (tagged) Agda types into Rust struct fields.
-- instance [(A.QName, A.Type)] ~>* R.StructField where


-- | Compiling Agda constructors into Rust variants.
instance (A.QName, A.Type) ~> R.Variant where
  go (c, ty) = inConstructor c $ do
    as <- vargTys ty
    RVariant (unqualR c) <$> goFs 1 (unDom <$> as)
    where
      goFs :: Int -> [(String, A.Type)] :~>* R.StructField
      goFs _ [] = return []
      goFs i ((x, ty):fs) =
        (:) <$> inArgument i (goF (x, ty))
            <*> A.addContext (A.defaultDom (x, ty)) (goFs (i + 1) fs)

      goF :: (String, A.Type) :~> R.StructField
      goF (x, ty) = RField <$> go ty

-- | Compiling (treeless) Agda terms into Rust expressions.
instance A.TTerm ~> R.Expr where
  go = boxTerm <=< (\case
    A.TVar i -> do
      report $ "* compiling variable: " <> pp i
      RExprRef . R.mkIdent <$> lookupCtxVar i
    A.TLit l -> RLit <$> go l
    t@(A.TDef qn) -> go (A.TApp t [])
    t@(A.TCon qn) -> go (A.TApp t [])
    A.TApp t ts -> do -- goHead t <*> gos (onlyNonErased ts)
      report $ "* compiling application: " <> pp t <> " $ " <> pp ts
      (cn, h) <- goHead t
      ts' <- maybe inNonConstructor inConstructor cn $ gos (onlyNonErased ts)
      return $ h ts'
    A.TLam t -> do
      ctx <- A.getContextTelescope
      report $ "ctx: " <> pp ctx
      panic "unsupported treeless term" (A.TLam t)
    A.TLet t t' -> do
      x <- freshVarInCtx
      let ty = undefined :: A.Type
      e  <- go t
      e' <- A.addContext [(x, A.defaultDom ty)] $ go t'
      return $ rLet [(x, e)] e'
    t@(A.TCase scrutinee _ defCase alts) -> do
      report $ "* compiling case expression:\n" <> pp t
      (x, ty) <- lookupCtx scrutinee
      isRec <- isJust <$> A.isRecordType ty
      report $ " scrutinee: " <> x
      report $ " scrutineeTy: " <> pp ty
      arms <- traverse go alts
      -- report $ " arms: " <> ppShow arms
      catchAll <- go defCase
      -- report $ " catchAll: " <> ppShow catchAll
      return $ RMatch (RExprRef $ R.mkIdent x)
                      (arms <> [RArm RWildP catchAll | not isRec])

    -- A.TUnit ->
    -- A.TSort ->
    -- A.TErased ->

    A.TCoerce t -> go t
    A.TError err -> do
      msg <- go $ A.LitString (T.pack $ ppShow err)
      return $ RMacroCall (RRef "panic") (RStrTok "IMPOSSIBLE")
    t -> panic "unsupported treeless term" t)
    where
    boxTerm :: R.Expr () :~> R.Expr
    boxTerm e = do
      toBox <- shouldBox
      return $ (if toBox then rBox else id) e

    goHead :: A.TTerm -> C (Maybe A.QName, ([R.Expr ()] -> R.Expr ()))
    goHead = \case
      A.TDef qn -> return (Nothing, RCall (unqualR qn))
      A.TCon cn -> do
        report $ "* compiling head of application (constructor: " <> pp cn <> ")"
        isRec <- A.isRecordConstructor cn
        report $ " isRec: " <> pp (isJust isRec)
        if isJust isRec then do
          Right A.ConHead{..} <- A.getConHead cn
          h <- parentQualR cn
          let xs = unqualR . unArg <$> conFields
          return (Just cn , \es -> RMkStruct h (zipWith (\x e -> R.Field x (Just e) ()) xs es))
        else do
          h <- qualR cn
          return (Just cn, \es -> RCallCon (RPathExpr h) es)
      A.TPrim prim | Just binOp <- getBinOp prim
        -> return (Nothing, \[x, y] -> RBin binOp x y)
        -- | A.PIf <- prim
        -- , [] <- xs
      A.TVar n -> do
        f <- R.mkIdent <$> lookupCtxVar n
        return (Nothing, RCall f)
      hd -> panic "unsupported head" (ppShow hd)

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

-- | Compiling *the* match clause in a case expression (records).

-- | Compiling match clauses in a case expression (datatypes).
instance A.TAlt ~> R.Arm where
  go = \case
    (A.TACon con n body) -> do
      report $ "* compiling arm (constructor): " <> pp con
      recCons <- recordConstructors <$> get
      report $ " recordConstructors: " <> pp recCons
      isRec <- isRecordConstructor con
      report $ " isRec: " <> pp isRec
      case isRec of
        -- compiling match on a record/struct value
        Just fs -> do
          path <- parentQualR con
          report $ " path: " <> ppR path
          (_, vargs, _) <- viewTy =<< A.typeOfConst con
          vargs' <- populateArgNames vargs
          report $ " vargs': " <> pp vargs'
          let xs = transcribe . fst . unDom <$> take n vargs'
          let pats = RId . R.mkIdent <$> xs
          report $ " pat: " <> pp con <> "(" <> show n <> ")"
                <> " ~> " <> ppR path <> "(" <> intercalate "," (map ppR pats) <> ")"
          body'  <- A.addContext vargs' (go body)
          report $ " body: " <> pp body <> " ~> " <> ppR body'
          -- return $ RArm (RStructP path $ zipWith RFieldP (R.mkIdent <$> fs) pats) body'
          return $ RArm (RStructP path $ RNoFieldP <$> pats) body'

        -- compiling match on a data/enum value
        Nothing -> do
          path <- qualR con
          (_, vargs, _) <- viewTy =<< A.typeOfConst con
          vargs' <- populateArgNames vargs
          let xs = take n (fst . unDom <$> vargs')
          let pats = RId . R.mkIdent <$> xs
          report $ " pat: " <> pp con <> "(" <> show n <> ")"
                <> " ~> " <> ppR path <> "(" <> intercalate "," (map ppR pats) <> ")"
          body'  <- unboxPats con n xs =<< A.addContext vargs' (go body)
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
    l -> panic "unsupported literal" l

-- * Agda utilities

currentCtx :: C [(String, A.Type)]
currentCtx = fmap (first pp . unDom) <$> A.getContext

reportCurrentCtx :: C ()
reportCurrentCtx = currentCtx >>= \ctx ->
  report $ "currentCtx: " <> pp ctx

lookupCtx :: Int -> C (String, A.Type)
lookupCtx i = first transcribe . (!! i) <$> currentCtx

currentCtxVars :: C [String]
currentCtxVars = fmap fst <$> currentCtx

lookupCtxVar :: Int -> C String
lookupCtxVar i = transcribe . (!! i) <$> currentCtxVars

varPool :: [String]
varPool = zipWith (<>) (repeat "x") (show <$> [0..])

getVarPool :: C [String]
getVarPool = do
  xs <- currentCtxVars
  return $ filter (`elem` xs) varPool

freshVar :: [String] -> String
freshVar xs = head $ dropWhile (`elem` xs) varPool

freshVarInCtx :: C String
freshVarInCtx = freshVar <$> currentCtxVars

onlyVisible :: A.LensHiding a => [a] -> [a]
onlyVisible = filter A.visible

onlyHidden :: A.LensHiding a => [a] -> [a]
onlyHidden = filter (not . A.visible)

vArgs :: [A.Arg a] -> [a]
vArgs = fmap unArg . onlyVisible

vElims :: [A.Elim] -> [A.Term]
vElims = vArgs . A.argsFromElims

isErased :: A.TTerm -> Bool
isErased = \case
  A.TErased -> True
  _         -> False

onlyNonErased :: [A.TTerm] -> [A.TTerm]
onlyNonErased = filter (not . isErased)

telListView :: A.Type -> C (A.ListTel, A.Type)
telListView t = do
  A.TelV tel t <- A.telViewPath t
  return (A.telToList tel, t)

viewTy :: A.Type -> C (A.ListTel, A.ListTel, A.Type)
viewTy ty = do
  (tel, resTy) <- telListView ty
  let (vargs, hargs) = partition A.visible tel
  return (hargs, vargs, resTy)

vargTys :: A.Type -> C A.ListTel
vargTys ty = do
  (_ , vargs, _) <- viewTy ty
  return vargs

resTy :: A.Type -> C A.Type
resTy ty = do
  (_ , _, ty) <- viewTy ty
  return ty

isData :: A.Type -> C Bool
isData ty = undefined
-- filterTel :: (Dom Type -> Bool) -> A.Telescope -> A.Telescope
-- filterTel p = \case
--   A.EmptyTel -> A.EmptyTel
--   A.ExtendTel a tel
--     (if p a then A.ExtendTel
--     | ->
--     | otherwise -> traverseF (filterTel p) tel

isDependentArrow :: A.Dom A.Type -> Bool
isDependentArrow ty = pp (A.domName ty) `notElem` ["_", "(nothing)"]

-- * Rust utilities

rLet :: [(String, R.Expr ())] -> R.Expr () -> R.Expr ()
rLet [] e = e
rLet xs e =
  let
    ps = map (\(x, e) -> R.Local (RId (R.mkIdent x)) Nothing (Just e) [] ()) xs
  in
    R.BlockExpr [] (R.Block (ps ++ [R.NoSemi e ()]) R.Normal ()) ()

pattern RBlock e = R.Block [R.NoSemi e ()] R.Normal ()
pattern RId x = R.IdentP (R.ByValue R.Immutable) x Nothing ()
pattern RArg x ty = R.Arg (Just (RId x)) ty ()
pattern RLit l = R.Lit [] l ()
pattern RMatch scr arms = R.Match [] scr arms ()

pattern RPathExpr p = R.PathExpr [] Nothing p ()
pattern RPathTy   p = R.PathTy      Nothing p ()
pattern RPathSegment x = R.PathSegment x Nothing ()

pattern RRef     x = R.Path False [RPathSegment x] ()
pattern RExprRef x = RPathExpr (RRef x)
pattern RTyRef   x = RPathTy   (RRef x)

pattern RConRef     x y = R.Path False [RPathSegment x, RPathSegment y] ()
pattern RExprConRef x y = RPathExpr (RConRef x y)
pattern RTyConRef   x y = RPathTy   (RConRef x y)

pattern RRef'   x ts = R.Path False [R.PathSegment x (Just (R.AngleBracketed [] ts [] ())) ()] ()
pattern RTyRef' x ts = RPathTy (RRef' x ts)

pattern RBin op x y = R.Binary [] op x y ()
pattern RAdd x y = RBin R.AddOp x y
pattern RDeref x = R.Unary [] R.Deref (RExprRef x) ()

pattern REmptyWhere = R.WhereClause [] ()
pattern RForall tys = R.Generics [] tys REmptyWhere ()
pattern REmptyGenerics = RForall []
pattern RTyParam x  = R.TyParam [] x [] Nothing ()
pattern RFnTy as b = R.FnDecl as (Just b) False ()
pattern RFn x ps ty b =
  R.Fn [] R.PublicV x ty R.Normal R.NotConst R.Rust (RForall ps) b ()
pattern RBareFn x a b =
  R.BareFn R.Normal R.Rust [] (RFnTy [ R.Arg (Just (RId x)) a ()] b) ()

pattern RCall f xs = R.Call [] (RExprRef f) xs ()
pattern RCallCon con xs = R.Call [] con xs ()
pattern RMkStruct path fs = R.Struct [] path fs Nothing ()
pattern RMacroCall f xs = R.MacExpr [] (R.Mac f xs ()) ()

pattern RNoSpan   = R.Span R.NoPosition R.NoPosition
pattern RTok    t = R.Tree (R.Token RNoSpan (R.LiteralTok t Nothing))
pattern RStrTok s = RTok (R.StrTok s)

pattern REnum x ps cs = R.Enum [] R.PublicV x cs (RForall ps) ()
pattern RVariant x fs = R.Variant x [] (R.TupleD fs ()) Nothing ()

pattern RStruct x ps fs = R.StructItem [] R.PublicV x (R.StructD fs ()) (RForall ps) ()
pattern RNamedField x ty = R.StructField (Just x) R.PublicV ty [] ()
pattern RField ty = R.StructField Nothing R.InheritedV ty [] ()

pattern RPointer ty = R.Rptr Nothing R.Immutable ty ()

pattern RWildP = R.WildP ()
pattern RLitP lit = R.LitP lit ()
pattern RFieldP x p = R.FieldPat (Just x) p ()
pattern RNoFieldP p = R.FieldPat Nothing p ()
pattern RTupleP path pats = R.TupleStructP path pats Nothing ()
pattern RStructP path fpats = R.StructP path fpats False ()
pattern RArm pat body = R.Arm [] (pat :| []) Nothing body ()
pattern RGuardedArm pat guard body = R.Arm [] (pat :| []) (Just guard) body ()

rBoxTy :: R.Ty () -> R.Ty ()
rBoxTy ty = RTyRef' (R.mkIdent "Box") [ ty ]

rBox :: R.Expr () -> R.Expr ()
rBox e = RCallCon (RExprConRef (R.mkIdent "Box") (R.mkIdent "new")) [ e ]

ppR :: (R.Pretty a, R.Resolve a) => a -> String
ppR = show . R.pretty'

-- * Utilities

enumerate :: [a] -> [(Int, a)]
enumerate = zip [1..]

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

report :: String -> C ()
report s = liftIO $ putStrLn s

panic :: (P.Pretty a, Show a) => String -> a -> b
panic s t = error $
  "[PANIC] unexpected " <> s <> ": " <> limit (pp t) <> "\n"
    <> "show: " <> limit (ppShow t)
  where limit = take 500

transcribe :: String -> String
transcribe ""     = error "[UNEXPECTED] empty identifier"
transcribe "_"    = error "[UNEXPECTED] placeholder identifier `_` should not be compiled"
transcribe (c:cs) = toXIDStart c <> concatMap toXIDContinue cs
  where
  -- TODO: malicious user can break compiler by reverse engineering an identifier
  -- * SOLUTION: keep a state of all produced identifiers and generate fresh ones
  toXIDStart, toXIDContinue :: Char -> String
  toXIDStart c
    | isXIDStart c || c == '_'
    = [c]
    | isXIDContinue c
    = "_" <> [c]
    | otherwise
    = toXIDContinue c

  toXIDContinue c
    | isXIDContinue c
    = [c]
    | otherwise
    = "Ֆ" <> show (ord c) <> "Ֆ"

unqual :: A.QName -> String
unqual = transcribe . pp . qnameName

unqualR :: A.QName -> R.Ident
unqualR = R.mkIdent . unqual

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
