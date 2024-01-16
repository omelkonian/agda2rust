{-# LANGUAGE FlexibleInstances, FlexibleContexts, OverloadedStrings #-}
-- | Conversion from Agda's internal syntax to our simplified JSON format.
module Agda2Rust where

-- import Control.Monad.Reader ( ReaderT, asks, liftIO )
import Control.Monad.IO.Class ( MonadIO(liftIO) )

import Data.List ( elemIndex )
-- import Data.Maybe ( maybeToList, fromMaybe )
-- import qualified Data.IntMap as M

-- import qualified Agda.Utils.VarSet as VS
-- import Agda.Utils.Monad ( ifM )
-- import Agda.Syntax.Common ( unArg )
import qualified Agda.Syntax.Common as A
import qualified Agda.Syntax.Internal as A
import qualified Agda.Syntax.Literal as A
import Agda.Syntax.Internal
  ( QName, absName, qnameName, qnameModule, unAbs, unEl, unDom
  , nameId, conName, dbPatVarIndex, pDom, telToList, telFromList )
import qualified Agda.TypeChecking.Substitute as A
  ( TelV(..) )
import qualified Agda.TypeChecking.Telescope as A
  ( telViewPath )

import Agda.Syntax.Translation.InternalToAbstract ( NamedClause(..) )
import qualified Agda.TypeChecking.Monad as A
import Agda.TypeChecking.Monad
  ( TCM, MonadTCM, liftTCM, typeOfConst, theDef, defName, getConstInfo
  , reportSLn, VerboseLevel )
import Agda.TypeChecking.Free ( freeVars, VarCounts(..) )
import Agda.TypeChecking.Level ( isLevelType )

-- import qualified Agda.Utils.Pretty as P
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


exampleRust = "\
\fn add(x: i32, y: i32) -> i32 {\
\x + y\
\}"

sourceFile :: R.SourceFile R.Span
sourceFile = R.parse' exampleRust

prettySource :: R.Doc ()
prettySource = R.pretty' sourceFile

-- | Converting between two types @a@ and @b@ under Agda's typechecking monad.
--
-- NB: 'go' is only used internally to de-clutter the recursive calls.
class (~>) a b | a -> b where
  convert, go :: a -> TCM (b ())
  convert = go

instance A.Definition ~> R.Item where
  go A.Defn{..} = goD theDef
   where
    goA :: (String, A.Type) -> TCM (R.Arg ())
    goA (x, ty) = do
      let pat = R.IdentP (R.ByValue R.Immutable) (R.mkIdent x) Nothing ()
      ty' <- go ty
      return $ R.Arg (Just pat) ty' ()

    goD :: A.Defn -> TCM (R.Item ())
    goD = \case
      A.AbstractDefn defn -> goD defn
      def@(A.Function{..}) -> do
        -- report $ show sourceFile <> "\n  ~>\n" <> show prettySource
        report $ "definition:\n" <> pp def
        -- report $ pp argTys <> "->" <> pp resTy
        A.TelV tel resTy <- A.telViewPath defType
        let argTys = unDom <$> filter A.visible (telToList tel)
            clause = head funClauses
        argTys' <- populateArgIds (getArgIds $ A.clauseTel clause)
               <$> traverse goA argTys
        resTy' <- go resTy
        block <- go clause
        return $ R.Fn
          []
          R.InheritedV
          (R.mkIdent $ unqualify defName)
          (R.FnDecl argTys' (Just resTy') False ())
          R.Normal
          R.NotConst
          R.Rust
          (R.Generics [] [] (R.WhereClause [] ()) ())
          block
          ()
    -- A.Function{..} -> let cls = takeWhile isNotCubical funClauses in
    --   -- NB: handle funWith and funExtLam
    --   Function <$> traverse go cls
    -- A.Datatype{..} -> do
    --   -- NB: what is a dataClause???
    --   tys <- traverse typeOfConst dataCons
    --   ADT <$> traverse go tys
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
    --       let Just ix = elemIndex (unqualify cn) (unqualify <$> dataCons)
    --       in  Constructor (pp conData) (toInteger ix)
    --     A.Record{..} -> Constructor (pp conData) 0
      d -> panic "unsupported definition" d
    -- A.Primitive{..}      -> return Primitive
    -- A.PrimitiveSort{..}  -> return Primitive
    -- A.Axiom{..}          -> return Postulate
    -- d@A.DataOrRecSig{..} -> panic "dataOrRecSig" d
    -- d@A.GeneralizableVar -> panic "generalizable variable" d

getArgIds :: A.Telescope -> [String]
getArgIds _ = ["x", "y"]

populateArgIds :: [String] -> [R.Arg ()] -> [R.Arg ()]
populateArgIds [] [] = []
populateArgIds (x:xs) (R.Arg (Just (R.IdentP b _             p ())) ty () : as)
                     = R.Arg (Just (R.IdentP b (R.mkIdent x) p ())) ty () : as'
                       where as' = populateArgIds xs as


instance A.Type ~> R.Ty where
  go ty = case unEl ty of
    -- Nat ~> i32
    A.Def n [] | pp n == "Agda.Builtin.Nat.Nat" -> return $
      R.PathTy Nothing (R.Path False [R.PathSegment "i32" Nothing ()] ()) ()
    -- _ -> _ ~> fn ...
    -- A.Pi a b | ->
    -- List _ ~> ?
    -- A.Def n as | pp n == "List", [a] <- vArgs as -> ?
    -- otherwise, error
    ty -> panic "unsupported type" ty

-- instance [A.Clause] ~> R.Block where
instance A.Clause ~> R.Block where
  go _ = return $
    (R.Block
       [R.NoSemi
         (R.Binary
            []
            R.AddOp
            (R.PathExpr [] Nothing (R.Path False [R.PathSegment "x" Nothing ()] ()) ())
            (R.PathExpr [] Nothing (R.Path False [R.PathSegment "y" Nothing ()] ()) ()) ())
            ()
       ]
       R.Normal
       ()
    )

-- instance A.Clause ~> Clause where
--   go A.Clause{..} =
--     Clause <$> go clauseTel
--            <*> traverse go (A.namedThing . unArg <$> namedClausePats)
--            -- ^ drop visibility and name information
--            <*> traverse go clauseBody

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

-- instance A.Type ~> Type where
--   go ty = do
--     let t = A.unEl ty
--     dl <- dependencyLevel t
--     Type <$> go t <*> ifM (asks includeDepLvls) (pure $ Just dl) (pure Nothing)

-- instance A.Term ~> Term where
--   go = flip (.) A.unSpine $ \case
--     -- ** abstractions
--     (A.Pi ty ab) -> do
--       ty' <- go (unEl $ unDom ty)
--       ab' <- go (unEl $ unAbs ab)
--       return $ Pi (isDependentArrow ty) (absName ab :~ ty') ab'
--     (A.Lam _ ab) -> do
--       ab' <- go (unAbs ab)
--       return $ Lam (pp (absName ab) :~ ab')
--     -- ** applications
--     (A.Var i   xs) -> App (DB i)                     <$> (traverse go xs)
--     (A.Def f   xs) -> App (Ref $ ppName f)           <$> (traverse go xs)
--     (A.Con c _ xs) -> App (Ref $ ppName $ conName c) <$> (traverse go xs)
--     -- ** other constants
--     (A.Lit   x)   -> return $ Lit   $ pp x
--     (A.Level x)   -> return $ Level $ pp x
--     (A.Sort  x)   -> return $ Sort  $ pp x
--     (A.MetaV _ _) -> return UnsolvedMeta
--     -- ** there are some occurrences of `DontCare` in the standard library
--     (A.DontCare t) -> go t
--     -- ** crash on the rest (should never be encountered)
--     t@(A.Dummy _ _) -> panic "term" t

-- instance A.Elim ~> Term where
--   go = \case
--     (A.Apply x)      -> go (unArg x)
--     (A.Proj _ qn)    -> return $ App (Ref $ ppName qn) []
--     (A.IApply _ _ x) -> go x

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
  "[PANIC] unexpected " <> s <> ": " <> pp t -- <> "\n show: " <> pp (show t)

ppName :: A.QName -> String
ppName qn = pp qn <> "<" <> show (fromEnum $ nameId $ qnameName qn) <> ">"

unqualify :: A.QName -> String
unqualify = pp . qnameName

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
        pinterleave " |" $ pbindings $ zip (unqualify <$> dataCons) tys
      A.Record{..} ->
        let (tel, fs) = splitAt recPars $ telToList recTel in
        (if null tel then "" else ppm (telFromList tel) <> " |- ")
          <> (braces $ pinterleave " ;" $ pbindings $ unDom <$> fs)
      A.Constructor{..} -> do
        let cn = conName conSrcCon
        d <- theDef <$> getConstInfo conData
        case d of
          A.Datatype{..} ->
            let Just ix = elemIndex (unqualify cn) (unqualify <$> dataCons)
            in  ppm conData <> "@" <> ppm ix
          A.Record{..} -> ppm conData <> "@0"
      A.Primitive{..}     -> "<Primitive>"
      A.PrimitiveSort{..} -> "<PrimitiveSort>"
      A.Axiom{..}         -> "<Axiom>"
      A.DataOrRecSig{..}  -> "<DataOrRecSig>"
      A.GeneralizableVar  -> "<GeneralizableVar>"



