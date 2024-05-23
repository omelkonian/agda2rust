-- | Convenient smart constructors for Rust constructs.
{-# LANGUAGE OverloadedStrings #-}
module Rust.Utils where

import Data.Generics      ( Data, listify )
import Data.List.NonEmpty ( NonEmpty((:|)) )

import Rust.Lib

-- ** blocks
pattern RBlock e = Block [NoSemi e ()] Normal ()

rLet :: [(String, Expr ())] -> Expr () -> Expr ()
rLet [] e = e
rLet xs e =
  let
    ps = map (\(x, e) -> Local (RId (mkIdent x)) Nothing (Just e) [] ()) xs
  in
    BlockExpr [] (Block (ps ++ [NoSemi e ()]) Normal ()) ()

pattern RMod x i = Mod [] PublicV x (Just [i]) ()

-- ** names & variables
pattern RId x = IdentP (ByValue Immutable) x Nothing ()

pattern RConRef     x y = RPath [RPathSeg x, RPathSeg y]
pattern RExprConRef x y = RPathExpr (RConRef x y)
pattern RTyConRef   x y = RPathTy   (RConRef x y)

pattern RPathExpr p = PathExpr [] Nothing p ()
pattern RPathTy   p = PathTy      Nothing p ()

pattern RPathSeg  x    = PathSegment x Nothing ()
pattern RPathSeg' x ts = PathSegment x (Just ts) ()

pattern RPath ps = Path False ps ()
pattern RRef     x = RPath [RPathSeg x]
-- pattern RRef' x ts = RPath [RPathSeg' x ts]
pattern RExprRef x = RPathExpr (RRef x)
pattern RExprRef' x ts = RPathExpr (RRef' x ts)
pattern RTyRef   x = RPathTy   (RRef x)
pattern RBorrow ty = Rptr Nothing Immutable ty ()

pattern RRef'   x ts = RPath [RPathSeg' x (RAngles ts)]
rRef' x ts | null ts   = RRef x
           | otherwise = RRef' x ts

pattern RTyRef' x ts = RPathTy (RRef' x ts)
rTyRef' x ts = RPathTy (rRef' x ts)

-- ** type parameters
pattern RParens ts = Parenthesized ts Nothing ()
pattern RParens' ts retTy = Parenthesized ts (Just retTy) ()
pattern RAngles ts = AngleBracketed [] ts [] ()
pattern RArg x ty = Arg (Just (RId x)) ty ()
pattern REmptyWhere = WhereClause [] ()
pattern RForall tys = Generics [] tys REmptyWhere ()
pattern REmptyGenerics = RForall []
pattern RTyParam x = TyParam [] x [] Nothing ()

rTyParam :: String -> TyParam ()
rTyParam = RTyParam . mkIdent

rTyFromExp :: Expr () -> Ty ()
rTyFromExp = \case
  RCall f [] -> rTyFromExp (RExprRef f)
  PathExpr [] self p () -> PathTy self p ()
  RBox e -> rTyFromExp e
  e -> error $ "[rTyFromExp] cannot type-convert " <> show e

-- ** type aliases
pattern RTyAlias x ty = TyAlias [] PublicV x ty REmptyGenerics ()
pattern RTyAlias' x ts ty = TyAlias [] PublicV x ty (RForall ts) ()

-- ** function types
pattern RFnTy as b = FnDecl as (Just b) False ()
pattern RBareFn x a b = BareFn Normal Rust [] (RFnTy [RArg x a] b) ()
rBareFn x a b | BareFn Normal Rust [] (RFnTy as b') () <- b
              = BareFn Normal Rust [] (RFnTy (RArg x a : as) b') ()
              | otherwise
              = RBareFn x a b
pattern RFn' isConst x ps ty b =
  Fn [] PublicV x ty Normal isConst Rust (RForall ps) b ()
pattern RFn x ps ty b = RFn' NotConst x ps ty b

-- ** function calls
pattern RCall f xs = Call [] (RExprRef f) xs ()
rCall f xs | null xs   = RExprRef f
           | otherwise = RCall f xs
pattern RCall' f ts xs = Call [] (RExprRef' f ts) xs ()
rCall' f ts xs | null ts   = RCall f xs -- rCall f xs
               | otherwise = RCall' f ts xs

pattern RCallCon con xs = Call [] con xs ()
pattern RMkStruct path fs = Struct [] path fs Nothing ()

pattern RMacroCall f xs = MacExpr [] (Mac f xs ()) ()
rPanic s = RMacroCall (RRef "panic") (RStrTok s)
rImpossible = rPanic "IMPOSSIBLE"
rUnreachable = RMacroCall (RRef "unreachable") RNoTok

-- ** traits
pattern RImpl tb = ImplTrait (tb :| []) ()
pattern RTyBound r = TraitTyParamBound (PolyTraitRef [] (TraitRef r) ()) None ()
pattern RImplFn a b = RImpl (RTyBound (RPath [RPathSeg' "Fn" (RParens' [a] b)]))
rImplFn a b | RImpl (RTyBound (RPath [RPathSeg' "Fn" (RParens' as b')])) <- b
            = RImpl (RTyBound (RPath [RPathSeg' "Fn" (RParens' (a : as) b')]))
            | otherwise
            = RImplFn a b

-- ** closures
pattern RInferArg x = RArg x (Infer ())
pattern RLam xs e = Closure [] Movable Ref (FnDecl xs Nothing False ()) e ()
rLam xs e = RLam (RInferArg <$> xs) e

-- ** constants
pattern RConstFn x ps ty b = RFn' Const x ps ty b
pattern RConst x ty b = ConstItem [] PublicV x ty b ()
pattern RStatic x ty b = Static [] PublicV x ty Immutable b ()

-- ** tokens & spans
pattern RNoSpan   = Span NoPosition NoPosition
pattern RTok    t = Tree (Token RNoSpan (LiteralTok t Nothing))
pattern RStrTok s = RTok (StrTok s)
pattern RNoTok    = Stream []

-- ** enums
pattern REnum x ps cs = Enum [] PublicV x cs (RForall ps) ()
pattern RVariant x fs = Variant x [] (TupleD fs ()) Nothing ()

-- ** structs
pattern RStruct x ps fs = StructItem [] PublicV x (StructD fs ()) (RForall ps) ()
pattern RNamedField x ty = StructField (Just x) PublicV ty [] ()
pattern RField ty = StructField Nothing InheritedV ty [] ()

-- ** patterns & match clauses
pattern RWildP = WildP ()
pattern RLitP lit = LitP lit ()
pattern RFieldP x p = FieldPat (Just x) p ()
pattern RNoFieldP p = FieldPat Nothing p ()
pattern RTupleP path pats = TupleStructP path pats Nothing ()
pattern RStructP path fpats = StructP path fpats False ()
pattern RArm pat body = Arm [] (pat :| []) Nothing body ()
pattern RGuardedArm pat guard body = Arm [] (pat :| []) (Just guard) body ()
pattern RMatch scr arms = Match [] scr arms ()

-- ** primitives
pattern RLit l = Lit [] l ()
rLit l | Str _ _ _ () <- l = MethodCall [] (RLit l) "to_string" Nothing [] ()
       | otherwise         = RLit l
pattern RLitBool b = RLit (Bool b Unsuffixed ())
pattern RLitTrue  = RLitBool True
pattern RLitFalse = RLitBool False

pattern RBin op x y = Binary [] op x y ()
pattern RAdd x y = RBin AddOp x y
pattern RDeref x = Unary [] Deref (RExprRef x) ()

-- ** pointers
pattern RPointer ty = Rptr Nothing Immutable ty ()
pattern RBoxTy   ty = RTyRef' "Box" [ ty ]
pattern RBox     e  = RCallCon (RExprConRef "Box" "new") [ e ]

-- ** phantom data
phantomField :: [Ident] -> Ty ()
phantomField ps
  = RPathTy
  $ RPath
    [ RPathSeg "std"
    , RPathSeg "marker"
    , RPathSeg' "PhantomData" (RAngles [TupTy (RTyRef <$> ps) ()])
    ]

-- ** pretty-printing
ppR :: (Pretty a, Resolve a) => a -> String
ppR = show . pretty'

-- ** tracking variable usage
idUses :: Data a => Ident -> a -> [Ident]
idUses n = listify (== n)

usedIn, unusedIn :: Data a => a -> Ident -> Bool
usedIn x n = not $ null $ idUses n x
unusedIn x = not . usedIn x
