-- | Convenient smart constructors for Rust constructs.
{-# LANGUAGE OverloadedStrings #-}
module Rust.Utils where

import Data.Generics      ( Data, listify )
import Data.List          ( intersperse )
import Data.List.NonEmpty ( NonEmpty((:|)) )
import Data.Maybe         ( isJust )

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

pattern RRef'   x ts = RPath [RPathSeg' x (RAngles ts)]
rRef' x ts | null ts   = RRef x
           | otherwise = RRef' x ts

pattern RTyRef' x ts = RPathTy (RRef' x ts)
rTyRef' x ts = RPathTy (rRef' x ts)

-- ** type bounds
pattern RTyBound r = TraitTyParamBound (PolyTraitRef [] (TraitRef r) ()) None ()
pattern RTyBoundRef s = RTyBound (RRef s)
pattern RTyBoundPath ps = RTyBound (RPath ps)

-- ** type parameters
pattern RParens ts = Parenthesized ts Nothing ()
pattern RParens' ts retTy = Parenthesized ts (Just retTy) ()
pattern RAngles ts = AngleBracketed [] ts [] ()
pattern RArg x ty = Arg (Just (RId x)) ty ()
pattern REmptyWhere = WhereClause [] ()
pattern RForall tys = Generics [] tys REmptyWhere ()
pattern REmptyGenerics = RForall []

pattern RTyParam' x bs = TyParam [] x bs Nothing ()
pattern RTyParam      x = RTyParam' x []
pattern RTyParamCopy  x = RTyParam' x [RTyBoundRef "Copy"]
pattern RTyParamClone x = RTyParam' x [RTyBoundRef "Clone"]
rTyParam      = RTyParam      . mkIdent
rTyParamCopy  = RTyParamCopy  . mkIdent
rTyParamClone = RTyParamClone . mkIdent

rTyFromExp :: Expr () -> Ty ()
rTyFromExp = \case
  RCall f [] -> rTyFromExp (RExprRef f)
  PathExpr [] self p () -> PathTy self p ()
  RBox e -> rTyFromExp e
  e -> error $ "[rTyFromExp] cannot type-convert " <> show e

rClone :: Expr () -> Expr ()
rClone e = RMethodCall e "clone"

-- ** type aliases
pattern RTyAlias x ty = TyAlias [] PublicV x ty REmptyGenerics ()
pattern RTyAlias' x ts ty = TyAlias [] PublicV x ty (RForall ts) ()

-- ** traits
pattern RTrait tb = TraitObject (tb :| []) ()
pattern RImpl tb = ImplTrait (tb :| []) ()
pattern RDyn tb = RTrait tb

-- ** function types
pattern RFn' isConst x ps ty b =
  Fn [] PublicV x ty Normal isConst Rust (RForall ps) b ()
pattern RFn x ps ty b = RFn' NotConst x ps ty b

pattern RFnTy as b = FnDecl as (Just b) False ()

rUnArg :: Arg a -> Ty a
rUnArg (Arg _ ty _) = ty

rMkArg :: Ty () -> Arg ()
rMkArg ty = Arg Nothing ty ()

mkImplFnTy, mkBoxFnTy, mkFnTy :: [Arg ()] -> Ty () -> Ty ()
mkImplFnTy = flip $ foldr (RImplFn . rUnArg)
mkBoxFnTy  = flip $ foldr (RBoxFn  . rUnArg)
mkFnTy     = mkBoxFnTy

mkFnDeclTy :: [Arg ()] -> Ty () -> FnDecl ()
-- mkFnDeclTy as b = RFnTy [] (mkFnTy as b) -- curried
mkFnDeclTy = RFnTy

pattern RDynFn a b = RDyn (RTyBoundPath [RPathSeg' "Fn" (RParens' [a] b)])
pattern RTraitFn a b = RTrait (RTyBoundPath [RPathSeg' "Fn" (RParens' [a] b)])
pattern RImplFn a b = RImpl (RTyBoundPath [RPathSeg' "Fn" (RParens' [a] b)])
pattern RBoxFn a b = RBoxTy (RDynFn a b)
pattern RRcFn a b = RRcTy (RDynFn a b)
pattern RBorrowFn a b = RBorrowTy (RDynFn a b)
rImplFn a b | RImpl (RTyBoundPath [RPathSeg' "Fn" (RParens' as b')]) <- b
            = RImpl (RTyBoundPath [RPathSeg' "Fn" (RParens' (a : as) b')])
            | otherwise
            = RImplFn a b
-- rMkFn = rImplFn
-- rMkFn = RBoxFn
-- rMkFn = RBorrowFn
rMkFn = RRcFn

pattern RBareFn x a b = BareFn Normal Rust [] (RFnTy [RArg x a] b) ()
rBareFn x a b | BareFn Normal Rust [] (RFnTy as b') () <- b
              = BareFn Normal Rust [] (RFnTy (RArg x a : as) b') ()
              | otherwise
              = RBareFn x a b

-- ** function calls
pattern RCall f xs = Call [] (RExprRef f) xs ()
rCall f xs | null xs   = RExprRef f
           | otherwise = rApply f xs
pattern RCall' f ts xs = Call [] (RExprRef' f ts) xs ()
rCall' f ts xs
  | null xs && null ts = RCall f xs
  | null ts   = rApply f xs
  | null xs   = RCall' f ts xs
  | otherwise = rApply' f ts xs

pattern RCallCon con xs = Call [] con xs ()
rCallCon con xs | null xs   = RCallCon con xs
                | otherwise = rApply con xs
pattern RMkStruct path fs = Struct [] path fs Nothing ()

pattern RMacroCall f xs = MacExpr [] (Mac f xs ()) ()
rPanic s = RMacroCall (RRef "panic") (RStrTok s)
rImpossible = rPanic "IMPOSSIBLE"
rUnreachable = RMacroCall (RRef "unreachable") RNoTok

rApply' f ts xs
  = rApply (RExprRef' f ts) xs

rApply :: (Pretty a, Resolve a) => a -> [Expr ()] -> Expr ()
rApply f xs
  -- | null xs
  -- = RCall f []
  -- | otherwise
  = RMacroCall (RRef "apply")
  $ Tree (Delimited RNoSpan NoDelim ts)
  where

  comma :: TokenStream
  comma = Tree $ Token RNoSpan Comma

  f' :: TokenStream
  f' = toTokenStream f

  args :: [TokenStream]
  args = map toTokenStream xs

  ts :: TokenStream
  ts = Stream $ intersperse comma (f':args)

-- ** closures
pattern RInferArg x = RArg x (Infer ())
pattern RLam' mov xs e = Closure [] Movable mov (FnDecl xs Nothing False ()) e ()
pattern RLam xs e = RLam' Ref xs e
pattern RMoveLam xs e = RLam' Value xs e
rLam' mov xs e = RLam' mov (RInferArg <$> xs) e
rLam = rLam' Ref
rMoveLam = rLam' Value

rMoveLams :: [Arg ()] -> Expr () -> Expr ()
rMoveLams = \case
  []     -> id
  (Arg (Just (IdentP _ x _ _)) _ _ : xs) -> RRc . rMoveLam [x] . rMoveLams xs

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
rFieldP x p | ppR x == ppR p = RNoFieldP p
            | otherwise      = RFieldP x p
pattern RTupleP path pats = TupleStructP path pats Nothing ()
pattern RStructP path fpats = StructP path fpats False ()
pattern RArm pat body = Arm [] (pat :| []) Nothing body ()
pattern RGuardedArm pat guard body = Arm [] (pat :| []) (Just guard) body ()
pattern RMatch scr arms = Match [] scr arms ()
rMatchClone scr arms = RMatch (rClone scr) arms
rMatchBorrow scr arms = RMatch (RBorrow scr) arms
rMatch = RMatch

-- ** methods
pattern RMethodCall' e f xs = MethodCall [] e f Nothing xs ()
pattern RMethodCall e f = RMethodCall' e f []

-- ** primitives
pattern RLit l = Lit [] l ()
rLit l | Str _ _ _ () <- l = RMethodCall (RLit l) "to_string"
       | otherwise         = RLit l
pattern RLitBool b = RLit (Bool b Unsuffixed ())
pattern RLitTrue  = RLitBool True
pattern RLitFalse = RLitBool False

pattern RBin op x y = Binary [] op x y ()
pattern RAdd x y = RBin AddOp x y
pattern RDeref x = Unary [] Deref (RExprRef x) ()

-- ** pointers
pattern RPointer ty = Rptr Nothing Immutable ty ()

pattern RBorrowTy ty = Rptr Nothing Immutable ty ()
pattern RBorrow   e  = AddrOf [] Immutable e ()

pattern RBoxTy ty = RTyRef' "Box" [ ty ]
-- pattern RBox   e  = RCallCon (RExprConRef "Box" "new") [ e ]
pattern RBox   e  = RCall "ᐁ" [ e ]

pattern RRcTy ty = RTyRef' "Rc" [ ty ]
-- pattern RRc   e  = RCallCon (RExprConRef "Rc" "new") [ e ]
pattern RRc   e  = RCall "ᐁF" [ e ]

rBox = RBox

-- ** phantom data
phantomField :: [Ident] -> Ty ()
phantomField ps
  = RPathTy
  $ RPath
    [ RPathSeg "std"
    , RPathSeg "marker"
    , RPathSeg' "PhantomData" (RAngles [TupTy (RTyRef <$> ps) ()])
    ]

mkPhantomField :: Expr ()
mkPhantomField
  = RPathExpr
  $ RPath
    [ RPathSeg "std"
    , RPathSeg "marker"
    , RPathSeg "PhantomData"
    ]

-- ** pretty-printing
ppR :: (Pretty a, Resolve a) => a -> String
ppR = show . pretty'

-- ** tokenization
toTokens :: (Pretty a, Resolve a) => a -> [Token]
toTokens x =
  let inp = inputStreamFromString (ppR x)
  in case execParser (lexTokens lexNonSpace) inp initPos of
    Left pf  -> error $ show pf
    Right ts -> unspan <$> ts

isDelim :: Token -> Bool
isDelim = \case { OpenDelim{} -> True; CloseDelim{} -> True; _ -> False }

isOpenDelim :: Token -> Maybe Delim
isOpenDelim = \case { OpenDelim d -> Just d; _ -> Nothing }

isCloseDelim :: Token -> Maybe Delim
isCloseDelim = \case { CloseDelim d -> Just d; _ -> Nothing }

toTokenStream :: (Pretty a, Resolve a) => a -> TokenStream
toTokenStream = Stream . mkTokenStream . toTokens
  where
  mkTokenStreamNoDelim :: [Token] -> TokenStream
  mkTokenStreamNoDelim = Stream . map (Tree . Token RNoSpan)

  closingDelim :: [Token] -> Delim -> Int -> [Delim] -> Int
  closingDelim [] _ _ _ = error "no closing delim"
  closingDelim (t:ts) d i ds
    -- closing our delimiter
    | Just d' <- isCloseDelim t
    , d' == d
    , ds == []
    = i
    -- closing other delimiter
    | Just d' <- isCloseDelim t
    , d'':ds' <- ds
    , d' == d''
    = closingDelim ts d (1 + i) ds'
    -- opening other delimiter
    | Just d' <- isOpenDelim t
    = closingDelim ts d (1 + i) (d':ds)
    -- non-delimiter token
    | otherwise
    = closingDelim ts d (1 + i) ds

  mkTokenStream :: [Token] -> [TokenStream]
  mkTokenStream ts
    -- | any isDelim ts
    | any (isJust . isOpenDelim) ts
    -- && any (isJust . isCloseDelim) ts
    = let
        (ts0, OpenDelim  d:ts')  = break (isJust . isOpenDelim) ts
        i = closingDelim ts' d 0 []
        (tsD, CloseDelim _:ts'') = splitAt i ts'
        -- (tsD, CloseDelim _:ts'') = break (== CloseDelim d) ts'
        -- (tsD, CloseDelim _:ts'') = break (isJust . isCloseDelim) ts'
      in
         [mkTokenStreamNoDelim ts0]
      <> [Tree $ Delimited RNoSpan d $ Stream $ mkTokenStream tsD]
      <> mkTokenStream ts''
    | otherwise
    = [mkTokenStreamNoDelim ts]

-- ** tracking variable usage
idUses :: Data a => Ident -> a -> [Ident]
idUses n = listify (== n)

usedIn, unusedIn :: Data a => a -> Ident -> Bool
usedIn x n = not $ null $ idUses n x
unusedIn x = not . usedIn x
