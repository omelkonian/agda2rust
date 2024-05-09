{-# LANGUAGE OverloadedStrings #-}
module Agda2Rust.Convert.Terms () where

import Control.Monad ( (<=<), filterM )
import Control.Monad.Error.Class ( MonadError(catchError) )
import Control.Monad.State ( get )

import Data.Maybe ( isJust )
import Data.List ( intercalate )

import Utils

import qualified Agda as A
import Agda.Lib ( ifM, ifJustM, mapMaybeM, downFrom )
import Agda.Utils

import qualified Rust as R
import Rust.Utils

import Agda2Rust.Pragma
import Agda2Rust.Monad
import Agda2Rust.Convert.Class
import Agda2Rust.Convert.Names
import Agda2Rust.Convert.Literals
import Agda2Rust.Convert.Builtins
import Agda2Rust.Convert.FFI
import Agda2Rust.Convert.Types

-- | Compiling (treeless) Agda terms into Rust expressions.
instance A.TTerm ~> R.Expr where
  go = boxTerm <=< (\case
    A.TVar i -> do
      -- iTy <- lookupCtxTy i
      -- report $ "* compiling variable: " <> pp i -- <> " : " <> pp iTy
      ctx <- currentCtx
      -- report $ " ctx: " <> pp ctx
      RExprRef . R.mkIdent <$> lookupCtxVar i
    A.TLit l -> rLit <$> go l
    t@(A.TDef _)  -> go (A.TApp t [])
    t@(A.TCon _)  -> go (A.TApp t [])
    t@(A.TPrim _) -> go (etaExpand 2 t)
      where
      etaExpand n t = A.mkTLam n $ A.raise n t `A.mkTApp` map A.TVar (downFrom n)
    A.TApp t (onlyNonErased -> ts) -> do
      report $ " * compiling application: " <> pp t <> " $ " <> pp ts
      (cn, h) <- goHead t
      (ts', ps) <- separateTyParams ts `catchError` \_ -> pure (ts, [])
      -- report $ "  ps: " <> pp ps
      -- report $ "  ts': " <> pp ts'
      ps' <- gos (typeFromTTerm <$> ps)
      -- report $ "  ps': " <> show ps'
      ts'' <- maybe inNonConstructor inConstructor cn $ gos ts'
      -- report $ "  ts'': " <> ppR ts''
      return $ h ps' ts''
    t0@(A.TLam t) -> do
      report $ " * compiling lambda term: " <> pp t0
      let (n, b) = A.tLamView t0
      report $ "  n: " <> pp n
      report $ "  b: " <> pp b
      xs <- freshVarsInCtx n
      report $ "  xs: " <> pp xs
      rLam (R.mkIdent <$> xs) <$> A.addContext (zip xs $ repeat defaultTy) (go b)
    t0@(A.TLet t t') -> do
      report $ " * compiling let: " <> pp t0
      x <- freshVarInCtx
      e  <- go t
      e' <- A.addContext [(x, defaultTy)] $ go t'
      return $ rLet [(x, e)] e'
    t@(A.TCase scrutinee _ defCase alts) -> do
      report $ " * compiling case expression:\n" <> pp t
      ctx <- currentCtx
      -- report $ "  ctx: " <> pp ctx
      -- report $ "  scrutineeVar: " <> pp scrutinee
      (x, ty) <- lookupCtx scrutinee -- (pred (length ctx) - scrutinee)
      -- report $ "  scrutinee: " <> x <> " : " <> pp ty
      arms <- traverse go =<< filterM shouldKeepAlt alts
      defArm <- case defCase of
        A.TError _ -> do
          -- report $ "  scrutineeTy: " <> pp ty
          shouldCatchAll <- A.reduce (A.unEl ty) >>= \case
            A.Def qn _ -> (A.theDef <$> A.getConstInfo qn) >>= \case
              A.Datatype{} -> hasUnusedTyParams qn
              _ -> return False
          return [RArm RWildP (rPanic "IMPOSSIBLE") | shouldCatchAll]
        t -> do
          catchAll <- go t
          return [RArm RWildP catchAll]
      return $ RMatch (RExprRef $ R.mkIdent x) (arms <> defArm)

    -- A.TUnit ->
    -- A.TSort ->
    -- A.TErased ->

    A.TCoerce t -> go t
    A.TError err -> do
      let msg = A.ppShow err
      return $ RMacroCall (RRef "panic") (RStrTok msg)
    t -> panic "treeless term" t)
    where
    shouldKeepAlt :: A.TAlt -> C Bool
    shouldKeepAlt = \case
      A.TACon{..} -> do
        cDef <- A.instantiateDef =<< A.getConstInfo aCon
        return $ hasQuantityNon0 cDef
      A.TAGuard{} -> pure True
      A.TALit{}   -> pure True

    boxTerm :: R.Expr () :~> R.Expr
    boxTerm e = do
      toBox <- shouldBox
      return $ (if toBox then RBox else id) e

    goHead :: A.TTerm -> C (Maybe A.QName, ([R.Ty ()] -> [R.Expr ()] -> R.Expr ()))
    goHead = \case
      A.TDef qn ->
        ifJustM (getFFI qn) (return . compileFFIHead) $ do
        ifM (isConst qn) (return $ compileFFIHead (Just Const, pp $ A.unqual qn)) $ do
          rn <- getRid <$> A.getConstInfo qn
          -- toConst <- isNullary =<< A.typeOfConst qn
          -- return (Nothing, (if toConst then rCall else RCall) (unqualR qn))
          return (Nothing, rCall' rn)
          where
          getRid :: A.Definition -> R.Ident
          getRid A.Defn{..}
            | Just (recName, recField) <- isRecordProjection theDef
            = unqualField recName recField
            | otherwise
            = unqualR defName
      A.TCon cn -> do
        report $ " * compiling head of application (constructor: " <> pp cn <> ")"
        isRec <- A.isRecordConstructor cn
        -- report $ "  isRec: " <> pp (isJust isRec)
        isBlt <- isBuiltinTerm cn
        if isJust isRec then do
          Right A.ConHead{..} <- A.getConHead cn
          h <- parentQualR cn
          let xs = unqualR . A.unArg <$> filter hasQuantityNon0 conFields
          return (Just cn , \[] es -> RMkStruct h (zipWith (\x e -> R.Field x (Just e) ()) xs es))
        else if isJust isBlt then do
          let Just bltE = isBlt
          return (Nothing, \[] [] -> compileBuiltinTerm bltE)
        else do
          h <- qualR cn
          return (Just cn, \[] es -> RCallCon (RPathExpr h) es)
      A.TPrim prim | Just binOp <- getBinOp prim ->
        return (Nothing, \[] [x, y] -> RBin binOp x y)
      A.TPrim A.PSeq ->
        return (Nothing, \[] -> last)
      A.TVar n -> do
        -- f <- R.mkIdent <$> lookupCtxVar n
        -- return (Nothing, rCall f)
        (f, ty) <- lookupCtx n
        toConst <- isNullary ty
        return (Nothing, \[] -> (if toConst then rCall else RCall) (R.mkIdent f))
      hd -> panic "head" (A.ppShow hd)

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
      report $ " * compiling arm (constructor): " <> pp con
      recCons <- recordConstructors <$> get
      -- report $ "  recordConstructors: " <> pp recCons
      isRec <- isRecordConstructor con
      -- report $ "  isRec: " <> pp isRec
      isBlt <- isBuiltinTerm con
      if isRec then do
        -- compiling match on a record/struct value
        path <- parentQualR con
        -- report $ "  path: " <> ppR path
        (_, vas, _) <- viewTy =<< A.typeOfConst con
        vas' <- populateArgNames vas
        -- report $ "  vas': " <> pp vas'
        let xs = transcribe . fst . A.unDom <$> filter hasQuantityNon0 (take n vas')
        Just (qn, _) <- A.isRecordConstructor con
        hasPhantomField <- hasUnusedTyParams qn
        let pats = RId . R.mkIdent <$> xs <> ["_phantom" | hasPhantomField]
        -- report $ "  pat: " <> pp con <> "(" <> show n <> ")"
        --       <> " ~> " <> ppR path <> "(" <> intercalate "," (map ppR pats) <> ")"
        body'  <- A.addContext vas' (go body)
        -- report $ "  body: " <> pp body <> " ~> " <> ppR body'
        return $ RArm (RStructP path (RNoFieldP <$> pats)) body'
      else case isBlt of
        -- compiling match on builtin value
        Just bltE -> do
          body' <- go body
          return $ RArm (RLitP $ compileBuiltinTerm bltE) body'
        -- compiling match on a data/enum value
        Nothing -> do
          path <- qualR con
          (_, vas, _) <- viewTy =<< A.typeOfConst con
          vas' <- populateArgNames vas
          let xs = take n (fst . A.unDom <$> filter hasQuantityNon0 vas')
          let pats = RId . R.mkIdent <$> xs
          -- report $ "  pat: " <> pp con <> "(" <> show n <> ")"
          --       <> " ~> " <> ppR path <> "(" <> intercalate "," (map ppR pats) <> ")"
          body'  <- unboxPats con n xs =<< A.addContext vas' (go body)
          -- report $ "  body: " <> pp body <> " ~> " <> ppR body'
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
        -- report $ "* unboxing " <> show xs
        ps <- flip mapMaybeM (enumerate xs) $ \(i, x) -> inArgument i $ do
          toBox <- shouldBox
          return $ if toBox then Just (x, RDeref (R.mkIdent x))
                            else Nothing
        return $ rLet ps e
    A.TAGuard guard body -> do
      report $ " * compiling arm (guard): " <> pp guard
      guard' <- go guard
      body'  <- go body
      return $ RGuardedArm RWildP guard' body'
    A.TALit lit body -> do
      report $ " * compiling arm (literal): " <> pp lit
      lit'  <- RLit <$> go lit
      body' <- go body
      return $ RArm (RLitP lit') body'
