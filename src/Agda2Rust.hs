{-# LANGUAGE FlexibleInstances, FlexibleContexts, OverloadedStrings #-}
-- | Conversion from Agda's internal syntax to our simplified JSON format.
module Agda2Rust where

import Control.Monad.IO.Class ( MonadIO(liftIO) )

import Data.List ( elemIndex )
import Data.Word ( Word64, Word8 )
import qualified Data.Text as T ( unpack )
import Data.Serializer ( toBytes )

import Agda.Syntax.Common ( unArg )
import qualified Agda.Syntax.Common as A
import qualified Agda.Syntax.Internal as A
import qualified Agda.Syntax.Literal as A
import Agda.Syntax.Internal
  ( QName, absName, qnameName, qnameModule, unAbs, unEl, unDom
  , nameId, conName, dbPatVarIndex, pDom, telToList, telFromList )
import qualified Agda.TypeChecking.Substitute as A
  ( TelV(..) )
import qualified Agda.TypeChecking.Telescope as A
  ( telViewPath, telViewUpTo, telViewUpTo' )

import Agda.Syntax.Translation.InternalToAbstract ( NamedClause(..) )
import qualified Agda.TypeChecking.Monad as A
import Agda.TypeChecking.Monad
  ( TCM, MonadTCM, liftTCM, typeOfConst, theDef, defName, getConstInfo
  , reportSLn, VerboseLevel )
import Agda.TypeChecking.Free ( freeVars, VarCounts(..) )
import Agda.TypeChecking.Level ( isLevelType )

import Agda.Syntax.Common.Pretty as P
  ( Pretty, prettyShow, renderStyle, Style(..), Mode(..) )
import Agda.TypeChecking.Pretty
  ( PrettyTCM(..), MonadPretty, fsep, punctuate, braces, parens, Doc )
import qualified Agda.TypeChecking.Pretty as P
  hiding (text)

import qualified Language.Rust.Syntax as R
import qualified Language.Rust.Data.Ident as R
import qualified Language.Rust.Parser as R
import qualified Language.Rust.Pretty as R

-- | Converting between two types @a@ and @b@ under Agda's typechecking monad.
--
-- NB: 'go' is only used internally to de-clutter the recursive calls.
class (~>) a b | a -> b where
  convert, go :: a -> TCM (b ())
  convert = go

pattern RBlock e = R.Block [R.NoSemi e ()] R.Normal ()
pattern RId x = R.IdentP (R.ByValue R.Immutable) x Nothing ()
pattern RArg x ty = R.Arg (Just (RId x)) ty ()
pattern RLit l = R.Lit [] l ()

pattern RRef     x = R.Path False [R.PathSegment x Nothing ()] ()
pattern RExprRef x = R.PathExpr [] Nothing (RRef x) ()
pattern RTyRef   x = R.PathTy   Nothing (RRef x) ()

pattern RRef'   x ts = R.Path False [R.PathSegment x (Just (R.AngleBracketed [] ts [] ())) ()] ()
pattern RTyRef' x ts = R.PathTy Nothing (RRef' x ts) ()

pattern RAdd x y = R.Binary [] R.AddOp x y ()

pattern REmptyWhere = R.WhereClause [] ()
pattern RForall tys = R.Generics [] tys REmptyWhere ()
pattern REmptyGenerics = RForall []
pattern RTyParam x  = R.TyParam [] x [] Nothing ()
pattern RFnTy as b = R.FnDecl as (Just b) False ()
pattern RFn x ty b =
  R.Fn [] R.InheritedV x ty R.Normal R.NotConst R.Rust REmptyGenerics b ()

pattern REnum x ps cs = R.Enum [] R.InheritedV x cs (RForall ps) ()
pattern RCall f xs = R.Call [] (RExprRef f) xs ()

pattern RVariant x fs = R.Variant x [] (R.TupleD fs ()) Nothing ()
-- pattern RField x ty = R.StructField (Just x) R.InheritedV ty [] ()
pattern RField ty = R.StructField Nothing R.InheritedV ty [] ()

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
    report $ "compiling definition: " <> pp defName
    goD theDef
    where
    dx = R.mkIdent (unqual defName)

    goA :: (String, A.Type) -> TCM (R.Arg ())
    goA (x, ty) = RArg (R.mkIdent x) <$> go ty

    goD :: A.Defn -> TCM (R.Item ())
    goD = \case
      A.AbstractDefn defn -> goD defn
      def@(A.Function{..}) -> do
        -- let cls = takeWhile isNotCubical funClauses in
        -- NB: handle funWith and funExtLam

        (argTys, resTy) <- viewTy defType
        let clause = head funClauses
        -- report $ pp argTys <> "->" <> pp resTy
        argTys' <- populateArgIds (getArgIds $ A.clauseTel clause)
               <$> traverse goA argTys
        RFn dx <$> (RFnTy argTys' <$> go resTy)
               <*> go clause
        where
        getArgIds :: A.Telescope -> [String]
        getArgIds = fmap unArg . A.telToArgs


        populateArgIds :: [String] -> [R.Arg ()] -> [R.Arg ()]
        populateArgIds [] [] = []
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

      goF :: (String, A.Type) -> TCM (R.StructField ())
      goF (x, ty) = RField <$> go ty

instance A.Type ~> R.Ty where
  go ty = case unEl ty of
    -- Nat ~> i32
    A.Def n [] | pp n == "Agda.Builtin.Nat.Nat" -> return (RTyRef "i32")
    -- _ -> _ ~> fn ...
    -- A.Pi a b | ->
    -- List _ ~> ?
    -- A.Def n as | pp n == "List", [a] <- vArgs as -> ?
    A.Def n es | as <- vArgs es -> do
      ctx <- A.getContextTelescope
      report $ "ctx: " <> pp ctx
      report $ "n: " <> pp n
      report $ "as: " <> pp as
      let sort = undefined :: A.Sort
      -- as' <-
      -- report $ "as': " <> show as'
      RTyRef' (unqualR n) <$> traverse go (A.El sort <$> as)
      -- return $ (PathTy
      --               Nothing
      --               (Path
      --                  False
      --                  [ PathSegment
      --                      (unqual n)
      --                      (Just
      --                         (AngleBracketed
      --                            []
      --                            [ PathTy Nothing (Path False [ PathSegment "T" Nothing () ] ()) ()
      --                            ]
      --                            []
      --                            ()))
      --                      ()
      --                  ]
      --                  ())
      --               ())
      -- where
        -- goParam :: -> [R.Ident]
    -- otherwise, error
    A.Var i es -> do
      x <- lookupCtxVar i
      -- es' <- traverse go (vArgs es)
      return $ RTyRef (R.mkIdent x)
    ty -> panic "unsupported type" ty

-- instance [A.Clause] ~> R.Block where
instance A.Clause ~> R.Block where
  go A.Clause{..} = case clauseBody of
    Nothing -> error "unsupported: empty clauses"
    Just e  -> A.addContext (A.KeepNames clauseTel)
             $ RBlock <$> go e

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

-- instance A.DeBruijnPattern ~> Pattern where
--   go = \case
--     A.VarP _ v -> return $ App (DB $ dbPatVarIndex v) []
--     A.DotP _ t -> go t
--     A.ConP c _ ps -> do
--       App (Ref $ pp c) <$> traverse go (A.namedThing . unArg <$> ps)
--     A.LitP _ lit -> return $ Lit (pp lit)
--     A.ProjP _ qn -> return $ App (Ref $ pp qn) []
--     p@(A.IApplyP _ _ _ _) -> panic "pattern (cubical)" p
--     p@(A.DefP _ _ _)      -> panic "pattern (cubical)" p

-- instance A.Telescope ~> Telescope where
--   go = traverse action . A.telToList
--     where
--     action :: A.Dom (Name, A.Type) -> TCM (Named (Pretty Type))
--     action dty = do
--       let (n, ty) = unDom dty
--       pty <- ppm ty
--       ty' <- go ty
--       let pdty = prender $ pDom dty $ P.text $ n <> " : " <> prender pty
--       return $ n :~ pdty :> ty'

-- instance A.Elim ~> Term where
--   go = \case
--     (A.Apply x)      -> go (unArg x)
--     (A.Proj _ qn)    -> return $ App (Ref $ ppName qn) []
--     (A.IApply _ _ x) -> go x

-- * Agda utilities

lookupCtxVar :: Int -> TCM String
lookupCtxVar i = do
  ctx <- fmap (fst . unDom) <$> A.getContext
  report $ "ctx: " <> pp ctx
  let x = pp (ctx !! i)
  report $ "ctx[" <> show i <> "]: " <> x
  return x

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



