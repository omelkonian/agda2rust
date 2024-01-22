{-# LANGUAGE FlexibleInstances, FlexibleContexts, OverloadedStrings #-}
-- | Conversion from Agda's internal syntax to our simplified JSON format.
module Agda2Rust where

import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Monad ( unless )
import Control.Arrow ( first )

import Data.List ( elemIndex )
import Data.List.NonEmpty ( NonEmpty((:|)) )
import Data.Word ( Word64, Word8 )
import qualified Data.Text as T ( unpack )
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
  ( QName, absName, qnameName, qnameModule, unAbs, unEl, unDom
  , nameId, conName, dbPatVarIndex, pDom, telToList, telFromList )
import Agda.Syntax.Translation.InternalToAbstract ( NamedClause(..) )
-- * treeless syntax
import qualified Agda.Compiler.ToTreeless as A
  ( toTreeless )
import qualified Agda.Syntax.Treeless as A
  ( TTerm(..), TPrim(..), TAlt(..), EvaluationStrategy(..), isPrimEq )
import Agda.Compiler.Treeless.Pretty ()
-- * typechecking
import qualified Agda.TypeChecking.Substitute as A
  ( TelV(..) )
import qualified Agda.TypeChecking.Telescope as A
  ( telViewPath, telViewUpTo, telViewUpTo' )
import qualified Agda.TypeChecking.Monad as A
import Agda.TypeChecking.Monad
  ( TCM, MonadTCM, liftTCM
  , theDef, defName
  , typeOfConst, getConstInfo
  , reportSLn, VerboseLevel )
import Agda.TypeChecking.Free ( freeVars, VarCounts(..) )
import Agda.TypeChecking.Level ( isLevelType )
import qualified Agda.TypeChecking.Datatypes as A
  ( getConstructorData, getConHead )
-- * pretty-printing
import Agda.Syntax.Common.Pretty as P
  ( Pretty, prettyShow, renderStyle, Style(..), Mode(..) )
import Agda.TypeChecking.Pretty
  ( PrettyTCM(..), MonadPretty, fsep, punctuate, braces, parens, Doc )
import qualified Agda.TypeChecking.Pretty as P
  hiding (text)
-- * utils
import Agda.Utils.Monad ( ifM )

-- * Rust
import qualified Language.Rust.Syntax as R
import qualified Language.Rust.Data.Ident as R
import qualified Language.Rust.Parser as R
import qualified Language.Rust.Pretty as R

-- | Converting between two types @a@ and @b@ under Agda's typechecking monad.
--
-- NB: 'go' is only used internally to de-clutter the recursive calls.
class (~>) a b | a -> b where
  convert, go :: a :~> b
  convert = go

type (:~>) a b = a -> TCM (b ())

ignoreDef :: A.Defn -> Bool
ignoreDef = \case
  A.Primitive{..} -> True
  A.PrimitiveSort{..} -> True
  A.Axiom{..} -> True
  A.DataOrRecSig{..} -> True
  A.GeneralizableVar -> True
  -- TODO
  -- A.Datatype{..} -> True
  A.Record{..} -> True
  A.Constructor{..} -> True
  _ -> False

viewTy :: A.Type -> TCM ([(String, A.Type)], A.Type)
viewTy ty = do
  A.TelV tel resTy <- A.telViewPath ty
  let argTys = unDom <$> onlyVisible (telToList tel)
  return (argTys, resTy)

vargTys :: A.Type -> TCM [(String, A.Type)]
vargTys = fmap fst . viewTy

resTy :: A.Type -> TCM A.Type
resTy = fmap snd . viewTy

instance A.Definition ~> R.Item where
  go A.Defn{..} = do
    report $ "*** compiling definition: " <> pp defName
    goD theDef
    where
    dx = R.mkIdent (unqual defName)

    goA :: (String, A.Type) :~> R.Arg
    goA (x, ty) = RArg (R.mkIdent x) <$> go ty

    goD :: A.Defn :~> R.Item
    goD = \case
      A.AbstractDefn defn -> goD defn

      -- A.Function{..} | d ^. funInline
      def@(A.Function{..}) -> do
        -- let cls = takeWhile isNotCubical funClauses in
        -- NB: handle funWith and funExtLam

        (argTys, resTy) <- viewTy defType
        report $ "resTy: " <> pp resTy
        report $ "argTys: " <> pp argTys
        let as = varPool
        argTys' <- populateArgIds as <$> traverse goA argTys
        report $ "argTys': " <> show argTys'

        Just tterm <- fmap stripTopTLams <$> A.toTreeless A.EagerEvaluation defName
        let tel = zip as (A.defaultDom . snd <$> argTys) :: [(String, A.Dom A.Type)]
        report $ "tel: " <> pp tel
        RFn dx <$> (RFnTy argTys' <$> go resTy)
               <*> A.addContext tel (RBlock <$> go tterm)
        where
        stripTopTLams :: A.TTerm -> A.TTerm
        stripTopTLams = \case
          A.TLam t -> stripTopTLams t
          t -> t

        populateArgIds :: [String] -> [R.Arg ()] -> [R.Arg ()]
        populateArgIds _ [] = []
        populateArgIds (x:xs) (RArg _ ty : as)
                             = RArg (R.mkIdent x) ty : populateArgIds xs as

      A.Datatype{..} -> do
        cs <- zip dataCons <$> traverse typeOfConst dataCons
        A.TelV tel _ <- A.telViewUpTo dataPars defType
        report $ "tel: " <> pp tel
        REnum dx <$> extractTyParams defType
                 <*> A.addContext tel (traverse go cs)
        where
        extractTyParams :: A.Type -> TCM [R.TyParam ()]
        extractTyParams ty = do
          xs <- traverse goA =<< vargTys ty
          report $ "xs: " <> pp xs
          return (RTyParam . R.mkIdent <$> xs)
          where
            goA :: (String, A.Type) -> TCM String
            goA (x, _) = return x
    -- A.Record{..} -> do
    --   -- NB: incorporate conHead/namedCon in the future for accuracy
    --   --     + to solve the issue with private (non-public) fields
    --   (tel, fs) <- splitAt recPars <$> go recTel
    --   return $ Record tel (thing . item <$> fs)
    -- A.Constructor{..} -> do
    --   let cn = conName conSrcCon
    --   d <- theDef <$> getConstInfo conData
    --   return $ case d of
    --     A.Datatype{..} ->
    --       let Just ix = elemIndex (unqual cn) (unqual <$> dataCons)
    --       in  Constructor (pp conData) (toInteger ix)
    --     A.Record{..} -> Constructor (pp conData) 0
      d -> panic "unsupported definition" d

instance (A.QName, A.Type) ~> R.Variant where
  go (c, ty) = do
    as <- vargTys ty
    RVariant (unqualR c) <$> goFs as
    where
      goFs :: [(String, A.Type)] -> TCM [R.StructField ()]
      goFs [] = return []
      goFs ((x, ty):fs) = do
        (:) <$> goF (x, ty)
            <*> A.addContext (A.defaultDom (x, ty)) (goFs fs)

      goF :: (String, A.Type) :~> R.StructField
      goF (x, ty) = RField <$> go ty

instance A.Type ~> R.Ty where
  go ty = case unEl ty of
    -- Nat ~> i32
    A.Def n as
      | Just primTy <- R.mkIdent <$> goPrim n
      -> do
        unless (null as) $ panic "primitive types cannot have type parameters" ty
        return $ RTyRef primTy
    A.Def n es | as <- vArgs es -> do
      ctx <- A.getContextTelescope
      report $ "ctx: " <> pp ctx
      report $ "n: " <> pp n
      report $ "as: " <> pp as
      -- RPointer .
      RTyRef' (unqualR n) <$> traverse go (A.El (undefined :: A.Sort) <$> as)

    -- A.Def n as | pp n == "List", [a] <- vArgs as -> ?

    -- A.Pi a b | ->

    -- otherwise, error
    A.Var i es -> do
      x <- lookupCtxVar i
      -- es' <- traverse go (vArgs es)
      return $ RTyRef (R.mkIdent x)
    ty -> panic "unsupported type" ty
    where
    goPrim :: QName -> Maybe String
    goPrim n = case pp n of
      "Agda.Builtin.Nat.Nat" -> Just "i32"
      _ -> Nothing

-- instance [A.Clause] ~> R.Block where
-- instance A.Clause ~> R.Block where
--   go A.Clause{..} = case clauseBody of
--     Nothing -> error "unsupported: empty clauses"
--     Just e  -> A.addContext (A.KeepNames clauseTel)
--              $ RBlock <$> go e

instance A.TAlt ~> R.Arm where
  go = \case
    A.TACon con n body -> do
      path <- qualR con
      let as   = varPool
          pats = RId . R.mkIdent <$> take n as
      (argTys, _) <- viewTy =<< A.typeOfConst con
      let tel = zip as (A.defaultDom . snd <$> argTys) :: [(String, A.Dom A.Type)]
      report $ "tel: " <> pp tel
      body' <- A.addContext tel $ go body
      return $ R.Arm [] (R.TupleStructP path pats Nothing () :| []) Nothing body' ()
    A.TAGuard guard body -> do
      guard' <- go guard
      body'  <- go body
      return $ R.Arm [] (R.WildP () :| []) (Just guard') body' ()
    A.TALit lit body -> do
      lit'  <- RLit <$> go lit
      body' <- go body
      return $ R.Arm [] (R.LitP lit' () :| []) Nothing body' ()

instance A.TTerm ~> R.Expr where
  go = \case
    A.TVar i -> RExprRef . R.mkIdent <$> lookupCtxVar i
    A.TLit l -> RLit <$> go l
    t@(A.TDef qn) -> go (A.TApp t [])
    t@(A.TCon qn) -> go (A.TApp t [])
    A.TApp t ts -> goHead t <*> traverse go ts
    A.TLam t -> do
      ctx <- A.getContextTelescope
      report $ "ctx: " <> pp ctx
      panic "unsupported treeless term" (A.TLam t)
    A.TLet t t' -> do
      x <- freshVarInCtx
      let pat = RId (R.mkIdent x) :: R.Pat ()
          ty  = undefined         :: A.Type
      e  <- go t
      let letStmt = R.Local pat Nothing (Just e) [] ()
      e' <- A.addContext [(x, A.defaultDom ty)] $ go t'
      return $ R.BlockExpr [] (R.Block [letStmt, R.NoSemi e' ()] R.Normal ()) ()
    t@(A.TCase scrutinee _ defCase alts) -> do
      report $ "* compiling case expression: " <> pp t
      scrutinee' <- RExprRef . R.mkIdent <$> lookupCtxVar scrutinee
      report $ "scrutinee: " <> show scrutinee'
      arms <- traverse go alts
      report $ "arms: " <> show arms
      catchAll <- if (case defCase of A.TError _ -> False; _ -> True)
        then (go defCase >>= \b -> return [R.Arm [] (R.WildP () :| []) Nothing b ()])
        else (return [])
      return $ R.Match [] scrutinee' (arms <> catchAll) ()

    -- A.TUnit ->
    -- A.TSort ->
    -- A.TErased ->
    A.TCoerce t -> go t
    A.TError err -> error $ show err
    t -> panic "unsupported treeless term" t
    where
    goHead :: A.TTerm -> TCM ([R.Expr ()] -> R.Expr ())
    goHead = \case
      A.TDef qn -> return $ RCall (unqualR qn)
      A.TCon qn -> RCallCon . RPathExpr <$> qualR qn
      A.TPrim prim
        | Just binOp <- getBinOp prim
        -> return $ \[x, y] -> RBin binOp x y
        -- | A.PIf <- prim
        -- , [] <- xs
        | otherwise
        -> panic "unsupported prim" (show prim)

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
      -- A.P64ToI ->

instance A.Term ~> R.Expr where
  go = flip (.) A.unSpine $ \case
    -- ** applications
    A.Def n es
      | pp n == "Agda.Builtin.Nat._+_"
      , [x, y] <- vArgs es
      -> RAdd <$> go x <*> go y
    A.Def n es ->
      RCall (unqualR n) <$> traverse go (vArgs es)
    A.Var i es -> do
      x <- lookupCtxVar i
      -- es' <- traverse go (vArgs es)
      return $ RExprRef (R.mkIdent x)

    -- ** abstractions
--     (A.Pi ty ab) -> do
--       ty' <- go (unEl $ unDom ty)
--       ab' <- go (unEl $ unAbs ab)
--       return $ Pi (isDependentArrow ty) (absName ab :~ ty') ab'
--     (A.Lam _ ab) -> do
--       ab' <- go (unAbs ab)
--       return $ Lam (pp (absName ab) :~ ab')
--     (A.Con c _ xs) -> App (Ref $ unqual $ conName c) <$> (traverse go xs)

    -- ** other constants
    A.Lit l -> RLit <$> go l
--     (A.Level x)   -> return $ Level $ pp x
--     (A.Sort  x)   -> return $ Sort  $ pp x

    -- ** there are some occurrences of `DontCare` in the standard library
    A.DontCare t -> go t

    -- ** crash on the rest (should never be encountered)
    e -> panic "unsupported term" e

instance A.Literal ~> R.Lit where
  go = return . \case
    A.LitNat    i -> R.Int R.Dec i R.Unsuffixed ()
    A.LitWord64 w -> R.ByteStr (toBytes w) R.Cooked R.Unsuffixed ()
    A.LitFloat  d -> R.Float d R.Unsuffixed ()
    A.LitString s -> R.Str (T.unpack s) R.Cooked R.Unsuffixed ()
    A.LitChar   c -> R.Char c R.Unsuffixed ()
    l -> panic "unsupported literal" l

-- * Agda utilities

currentCtx :: TCM [(String, A.Type)]
currentCtx = fmap (first pp . unDom) <$> A.getContext

currentCtxVars :: TCM [String]
currentCtxVars = fmap fst <$> currentCtx

lookupCtxVar :: Int -> TCM String
lookupCtxVar i = do
  ctx <- currentCtxVars
  report $ "ctx: " <> pp ctx
  let x = pp (ctx !! i)
  report $ "ctx[" <> show i <> "]: " <> x
  return x

varPool :: [String]
varPool = zipWith (<>) (repeat "x") (show <$> [0..])

freshVar :: [String] -> String
freshVar xs = head $ dropWhile (`elem` xs) varPool

freshVarInCtx :: TCM String
freshVarInCtx = freshVar <$> currentCtxVars

onlyVisible :: A.LensHiding a => [a] -> [a]
onlyVisible = filter A.visible

vArgs :: [A.Elim] -> [A.Term]
vArgs = fmap unArg . onlyVisible . A.argsFromElims

-- filterTel :: (Dom Type -> Bool) -> A.Telescope -> A.Telescope
-- filterTel p = \case
--   A.EmptyTel -> A.EmptyTel
--   A.ExtendTel a tel
--     (if p a then A.ExtendTel
--     | ->
--     | otherwise -> traverseF (filterTel p) tel

-- * Rust utilities

pattern RBlock e = R.Block [R.NoSemi e ()] R.Normal ()
pattern RId x = R.IdentP (R.ByValue R.Immutable) x Nothing ()
pattern RArg x ty = R.Arg (Just (RId x)) ty ()
pattern RLit l = R.Lit [] l ()

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

pattern REmptyWhere = R.WhereClause [] ()
pattern RForall tys = R.Generics [] tys REmptyWhere ()
pattern REmptyGenerics = RForall []
pattern RTyParam x  = R.TyParam [] x [] Nothing ()
pattern RFnTy as b = R.FnDecl as (Just b) False ()
pattern RFn x ty b =
  R.Fn [] R.InheritedV x ty R.Normal R.NotConst R.Rust REmptyGenerics b ()

pattern REnum x ps cs = R.Enum [] R.InheritedV x cs (RForall ps) ()
pattern RCall f xs = R.Call [] (RExprRef f) xs ()
pattern RCallCon con xs = R.Call [] con xs ()

pattern RVariant x fs = R.Variant x [] (R.TupleD fs ()) Nothing ()
-- pattern RField x ty = R.StructField (Just x) R.InheritedV ty [] ()
pattern RField ty = R.StructField Nothing R.InheritedV ty [] ()

pattern RPointer ty = R.Rptr Nothing R.Immutable ty ()

-- * Utilities

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

report :: String -> TCM ()
report s = liftIO $ putStrLn s

panic :: (P.Pretty a, Show a) => String -> a -> b
panic s t = error $
  "[PANIC] unexpected " <> s <> ": " <> limit (pp t) <> "\n"
    <> "show: " <> limit (pp $ show t)
  where limit = take 500

unqual :: A.QName -> String
unqual = pp . qnameName

unqualR :: A.QName -> R.Ident
unqualR = R.mkIdent . transcribe . unqual

transcribe :: String -> String
transcribe "[]" = "nil"
transcribe "_∷_" = "cons"
transcribe s = s

qualR :: A.QName :~> R.Path
qualR qn = do
  Right con <- fmap A.conName <$> A.getConHead qn
  ty  <- A.getConstructorData qn
  return $ RConRef (unqualR ty) (unqualR con)

(\/) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(f \/ g) x = f x || g x

isNotCubical :: A.Clause -> Bool
isNotCubical A.Clause{..}
  | Just (A.Def qn _) <- clauseBody
  = pp (qnameModule qn) /= "Agda.Primitive.Cubical"
  | otherwise
  = True

instance P.PrettyTCM A.Definition where
  prettyTCM d = go (theDef d)
   where
    go = \case
      A.AbstractDefn defn -> go defn
      A.Function{..} -> let cls = takeWhile isNotCubical funClauses in
        fsep $ punctuate " |"
             $ ppm . NamedClause (defName d) True <$> cls
      A.Datatype{..} -> do
        tys <- fmap unEl <$> traverse typeOfConst dataCons
        pinterleave " |" $ pbindings $ zip (unqual <$> dataCons) tys
      A.Record{..} ->
        let (tel, fs) = splitAt recPars $ telToList recTel in
        (if null tel then "" else ppm (telFromList tel) <> " |- ")
          <> (braces $ pinterleave " ;" $ pbindings $ unDom <$> fs)
      A.Constructor{..} -> do
        let cn = conName conSrcCon
        d <- theDef <$> getConstInfo conData
        case d of
          A.Datatype{..} ->
            let Just ix = elemIndex (unqual cn) (unqual <$> dataCons)
            in  ppm conData <> "@" <> ppm ix
          A.Record{..} -> ppm conData <> "@0"
      A.Primitive{..}     -> "<Primitive>"
      A.PrimitiveSort{..} -> "<PrimitiveSort>"
      A.Axiom{..}         -> "<Axiom>"
      A.DataOrRecSig{..}  -> "<DataOrRecSig>"
      A.GeneralizableVar  -> "<GeneralizableVar>"



