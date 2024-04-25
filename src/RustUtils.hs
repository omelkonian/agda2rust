-- | Rust utilities.
module RustUtils where

import Data.Generics      ( Data, listify )
import Data.List.NonEmpty ( NonEmpty((:|)) )

import Language.Rust.Syntax
import Language.Rust.Data.Ident
import Language.Rust.Data.Position
import Language.Rust.Pretty

rLet :: [(String, Expr ())] -> Expr () -> Expr ()
rLet [] e = e
rLet xs e =
  let
    ps = map (\(x, e) -> Local (RId (mkIdent x)) Nothing (Just e) [] ()) xs
  in
    BlockExpr [] (Block (ps ++ [NoSemi e ()]) Normal ()) ()

pattern RBlock e = Block [NoSemi e ()] Normal ()
pattern RId x = IdentP (ByValue Immutable) x Nothing ()
pattern RArg x ty = Arg (Just (RId x)) ty ()
pattern RLit l = Lit [] l ()
pattern RMatch scr arms = Match [] scr arms ()

pattern RPathExpr p = PathExpr [] Nothing p ()
pattern RPathTy   p = PathTy      Nothing p ()

pattern RPathSeg  x    = PathSegment x Nothing ()
pattern RPathSeg' x ts = PathSegment x (Just ts) ()

pattern RPath ps = Path False ps ()
pattern RRef     x = RPath [RPathSeg x]
pattern RExprRef x = RPathExpr (RRef x)
pattern RTyRef   x = RPathTy   (RRef x)

pattern RConRef     x y = RPath [RPathSeg x, RPathSeg y]
pattern RExprConRef x y = RPathExpr (RConRef x y)
pattern RTyConRef   x y = RPathTy   (RConRef x y)

pattern RAngles   ts = AngleBracketed [] ts [] ()
-- pattern RRef'   x ts = RPath [RPathSeg' x (RAngles ts)]
-- pattern RTyRef' x ts = RPathTy (RRef' x ts)
rRef' x ts
  | null ts   = RRef x
  | otherwise = RPath [RPathSeg' x (RAngles ts)]
rTyRef' x ts = RPathTy (rRef' x ts)

pattern RBin op x y = Binary [] op x y ()
pattern RAdd x y = RBin AddOp x y
pattern RDeref x = Unary [] Deref (RExprRef x) ()

pattern REmptyWhere = WhereClause [] ()
pattern RForall tys = Generics [] tys REmptyWhere ()
pattern REmptyGenerics = RForall []
pattern RTyParam x  = TyParam [] x [] Nothing ()
pattern RFnTy as b = FnDecl as (Just b) False ()
pattern RBareFn x a b =
  BareFn Normal Rust [] (RFnTy [ Arg (Just (RId x)) a ()] b) ()
pattern RFn x ps ty b =
  Fn [] PublicV x ty Normal NotConst Rust (RForall ps) b ()
pattern RConst x ty b = ConstItem [] PublicV x ty b ()

-- pattern RCall f xs = Call [] (RExprRef f) xs ()
rCall f xs
  | null xs   = RExprRef f
  | otherwise = Call [] (RExprRef f) xs ()
pattern RCallCon con xs = Call [] con xs ()
pattern RMkStruct path fs = Struct [] path fs Nothing ()
pattern RMacroCall f xs = MacExpr [] (Mac f xs ()) ()

pattern RNoSpan   = Span NoPosition NoPosition
pattern RTok    t = Tree (Token RNoSpan (LiteralTok t Nothing))
pattern RStrTok s = RTok (StrTok s)

pattern REnum x ps cs = Enum [] PublicV x cs (RForall ps) ()
pattern RVariant x fs = Variant x [] (TupleD fs ()) Nothing ()

pattern RStruct x ps fs = StructItem [] PublicV x (StructD fs ()) (RForall ps) ()
pattern RNamedField x ty = StructField (Just x) PublicV ty [] ()
pattern RField ty = StructField Nothing InheritedV ty [] ()

pattern RPointer ty = Rptr Nothing Immutable ty ()

pattern RWildP = WildP ()
pattern RLitP lit = LitP lit ()
pattern RFieldP x p = FieldPat (Just x) p ()
pattern RNoFieldP p = FieldPat Nothing p ()
pattern RTupleP path pats = TupleStructP path pats Nothing ()
pattern RStructP path fpats = StructP path fpats False ()
pattern RArm pat body = Arm [] (pat :| []) Nothing body ()
pattern RGuardedArm pat guard body = Arm [] (pat :| []) (Just guard) body ()

rBoxTy :: Ty () -> Ty ()
rBoxTy ty = rTyRef' (mkIdent "Box") [ ty ]

rBox :: Expr () -> Expr ()
rBox e = RCallCon (RExprConRef (mkIdent "Box") (mkIdent "new")) [ e ]

ppR :: (Pretty a, Resolve a) => a -> String
ppR = show . pretty'

idUses :: Data a => Ident -> a -> [Ident]
idUses n = listify (== n)

usedIn, unusedIn :: Data a => a -> Ident -> Bool
usedIn x n = not $ null $ idUses n x
unusedIn x = not . usedIn x
