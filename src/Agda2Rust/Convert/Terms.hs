{-# LANGUAGE OverloadedStrings #-}
module Agda2Rust.Convert.Terms () where

import Control.Monad ( (<=<), filterM, when )
import Control.Monad.Error.Class ( MonadError(catchError) )
import Control.Monad.State ( get )
import Control.Monad.Reader ( asks )

import Data.Maybe ( isJust, isNothing, fromJust )
import Data.List ( intercalate, splitAt )

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
  go t = do
   -- report $ "* compiling tterm: " <> pp t
   case t of
    t0@(A.TVar i) -> do
      -- iTy <- lookupCtxTy i
      -- report $ "* compiling variable: " <> pp i <> " : " <> pp iTy
      -- reportCurrentCtx
      -- shouldEtaExpand <- do
      --   ar <- arityOf t
      --   return $ case ar of
      --     Nothing  -> Nothing
      --     Just arN -> if arN > 0 then Just arN else Nothing
      -- case shouldEtaExpand of
      --   Just etaN -> do
      --     intros <- asks funIntroVars
      --     inNoFunIntros $ go (etaExpandT etaN intros t0)
      --   Nothing -> RExprRef . R.mkIdent <$> lookupCtxVar i
      RExprRef . R.mkIdent <$> lookupCtxVar i
    A.TLit l -> rLit <$> go l
    t0@(A.TLam t) -> {-| 0 `A.freeIn` t-} {-inNoFunIntros $ -}do
      report $ " * compiling lambda term: " <> pp t0
      let (n, b) = A.tLamView t0
      report $ "  n: " <> pp n
      report $ "  b: " <> pp b
      intros <- asks funIntroVars
      report $ "  intros: " <> pp intros
      localIntros <- asks localIntroVars
      report $ "  localIntros: " <> pp localIntros

      if intros >= n then do
        let b' = A.raiseFrom 1 (-1) b
        report $ "  b': " <> pp b'
        go b'
      else if intros == 0 then do
        xs <- freshVarsInCtx n
        report $ "  xs: " <> pp xs
        body <- A.addContext (zip xs $ repeat defaultTy) (go b)
        return $ rMoveLams (RInferArg . R.mkIdent <$> xs) body
      else
        error "INTROS!=N!=0"

    t0@(A.TLet t t') -> do
      report $ " * compiling let: " <> pp t0
      x <- freshVarInCtx "_"
      e  <- go t
      e' <- A.addContext [(x, defaultTy)] $ go t'
      return $ rLet [(x, e)] e'
    t@(A.TCase scrutinee A.CaseInfo{..} defCase alts) -> {-inNoFunIntros $-} do
      report $ " * compiling case expression:\n" <> pp t
      intros <- asks funIntroVars
      report $ "  intros: " <> pp intros
      ctx <- currentCtx
      report $ "  ctx: " <> pp ctx
      report $ "  scrutineeVar: " <> pp scrutinee
      (x, ty) <- lookupCtx scrutinee -- (pred (length ctx) - scrutinee)
      report $ "  scrutinee: " <> x <> " : " <> pp ty
      arms <- traverse go =<< filterM shouldKeepAlt alts
      defArm <- case defCase of
        A.TError _ -> do
          -- report $ "  scrutineeTy: " <> pp ty
          shouldCatchAll <- case caseType of
            A.CTData qn ->
              (&&) <$> (isDataDef . A.theDef <$> A.getConstInfo qn)
                   <*> hasUnusedTyParams qn
            _ -> return False
          return [RArm RWildP rUnreachable | shouldCatchAll]
        t -> do
          catchAll <- go t
          return [RArm RWildP catchAll]
      return $ rMatch (RExprRef $ R.mkIdent x) (arms <> defArm)

    -- A.TUnit -> return $ RExprRef "_"
    -- A.TErased -> return $ RExprRef "_"
    -- A.TSort ->
    -- A.TCoerce t -> go t

    A.TError err -> do
      let msg = A.ppShow err
      return $ RMacroCall (RRef "panic") (RStrTok msg)

    t@(A.TDef _)  -> go (A.TApp t [])
    t@(A.TCon _)  -> go (A.TApp t [])
    t@(A.TPrim _) -> go (A.TApp t [])
    t0@(A.TApp t ts0@(onlyNonErased -> ts)) -> do
      report $ " * compiling application: " <> pp t <> " $ " <> pp ts0
      -- ty <- getTypeT t
      -- report $ "  ty: " <> pp ty
      -- ty0 <- getTypeT t0
      -- report $ "  ty0: " <> pp ty0
      ar <- arityOf t
      report $ "  arity(t): " <> pp ar
      -- appAr <- arityOf t0
      -- report $ "  arity(t0): " <> pp appAr

      (cn, h) <- goHead t
      -- report $ "   cn: " <> pp cn

      (ts', ps) <- separateTyParams ts0 -- `catchError` \_ -> pure (ts, [])
      report $ "  ps: " <> pp ps
      report $ "  ts': " <> pp ts'

      shouldEtaExpand <- do
        return $ case ar of
          Nothing  -> Nothing
          Just arN -> if d > 0 then Just d else Nothing
            where d = arN - length ts'
      case shouldEtaExpand of
        Just etaN -> do
          report $ "  etaN: " <> pp etaN
          intros <- asks funIntroVars
          report $ "  intros: " <> pp intros
          localIntros <- asks localIntroVars
          report $ "  localIntros: " <> pp localIntros
          let et = etaExpandT etaN intros localIntros t0
          report $ "  η: " <> pp t0 <> " ~> " <> pp et
          inNoFunIntros $ go et
        Nothing -> inNoFunIntros $ do
          -- report $ "  ps: " <> pp ps
          -- report $ "  ts': " <> pp ts'
          ps' <- gos (typeFromTTerm <$> ps)
          -- report $ "  ps': " <> show ps'
          {-aTys <- A.liftTCM $ getArgTypesT t ts
          report $ "  aTys: " <> pp aTys-}
          ts'' <- maybe inNonConstructor inConstructor cn
                -- $ inArgTypes {-aTys -}undefined
                $ gos ts'
          -- report $ "  ts'': " <> ppR ts''
          toBox <- shouldBox
          return $ (if toBox then rBox else id) (h ps' ts'')
    t -> panic "treeless term" t
    where
    shouldKeepAlt :: A.TAlt -> C Bool
    shouldKeepAlt = \case
      A.TACon{..} -> do
        cDef <- A.instantiateDef =<< A.getConstInfo aCon
        return $ hasQuantityNon0 cDef
      A.TAGuard{} -> pure True
      A.TALit{}   -> pure True

    goHead :: A.TTerm
           -> C (Maybe A.QName , ([R.Ty ()] -> [R.Expr ()] -> R.Expr ()))
    goHead = \case
      A.TDef qn ->
        ifJustM (getFFI qn) (return . compileFFIHead) $ do
        ifM (isConst qn) (return $ compileFFIHead (Just Const, pp $ A.unqual qn)) $ do
          report $ " * compiling head of application (definition: " <> pp qn <> ")"
          rn <- getRid <$> A.getConstInfo qn
          -- toConst <- isNullary =<< A.typeOfConst qn
          -- return (Nothing, (if toConst then rCall else RCall) (unqualR qn))
          return (Nothing, \ts -> rCall' rn ts)
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
          let Just (qn, _) = isRec
          hasPhantomField <- hasUnusedTyParams qn
          return (Just cn, \[] es ->
            RMkStruct h (zipWith (\x e -> R.Field x (Just e) ()) xs es
                          <> [R.Field "_phantom" (Just mkPhantomField) () | hasPhantomField]))
        else if isJust isBlt then do
          let Just bltE = isBlt
          return (Nothing, \[] [] -> compileBuiltinTerm bltE)
        else do
          h <- qualR cn
          return (Just cn, \[] es -> rCallCon (RPathExpr h) es)
      A.TPrim prim | Just binOp <- getBinOp prim ->
        return (Nothing, \[] [x, y] -> RBin binOp x y)
      A.TPrim A.PSeq ->
        return (Nothing, \[] -> last)
      A.TVar n -> do
        -- f <- R.mkIdent <$> lookupCtxVar n
        -- return (Nothing, rCall f)
        (f, ty) <- lookupCtx n
        toConst <- isNullary ty
        return (Nothing, \[] -> (if toConst then rCall else rApply) (R.mkIdent f))
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
      -- A.PITo64 ->

    arityOf :: A.TTerm -> C (Maybe Int)
    arityOf t = {-report ("  arityOf: " <> pp t) >>-} case t of
      A.TDef n  -> getArity n
      A.TCon n  -> getArity n
      A.TPrim _ -> return $ Just 2 -- TODO: generalize this to 1/2/3
      A.TVar i  -> Just . erasedArity <$> lookupCtxTy i
      A.TApp t ts -> do
        (ts, _) <- separateTyParams ts
        Just headAr <- arityOf t
        report $ "    headAr: " <> pp headAr
        let ar = headAr - length ts
        report $ "    ar: " <> pp ar
        when (ar < 0) $ error "[arityOf] negative arity!"
        return $ Just ar
      A.TLam t -> fmap (1 +) <$> arityOf t
      _ -> panic "tterm [arityOf]" t

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
        -- * compiling match on a record/struct value
        path <- parentQualR con
        -- report $ "  path: " <> ppR path
        (tel, _) <- telListView =<< A.typeOfConst con
        -- report $ " tel: " <> pp tel
        tel' <- populateArgNames tel
        -- report $ " tel': " <> pp tel'
        ctx <- filterM (fmap not . isSrtOrLvlTy . snd . A.unDom) tel
        bodyCtx <- filterM (fmap not . isSrtOrLvlTy . snd . A.unDom) tel'
        let fs = transcribe . fst . A.unDom <$> filter hasQuantityNon0 (take n $ ctx)
        let xs = transcribe . fst . A.unDom <$> filter hasQuantityNon0 (take n $ bodyCtx)
        Just (qn, _) <- A.isRecordConstructor con
        hasPhantomField <- hasUnusedTyParams qn
        let phantomField = ["_phantom" | hasPhantomField]
        -- TODO: hygienic handling of hardcoded string "_phantom"
        let pfs  = R.mkIdent       <$> fs <> phantomField
        let pats = RId . R.mkIdent <$> xs <> phantomField
        -- report $ "  pat: " <> pp con <> "(" <> show n <> ")"
        --       <> " ~> " <> ppR path <> "(" <> intercalate "," (map ppR pats) <> ")"
        body' <- A.addContext bodyCtx $ inLocalIntros (length pats) $ go body
        -- report $ "  body: " <> pp body <> " ~> " <> ppR body'
        return $ RArm (RStructP path (uncurry rFieldP <$> zip pfs pats)) body'
      else case isBlt of
        -- * compiling match on builtin value
        Just bltE -> do
          body' <- go body
          return $ RArm (RLitP $ compileBuiltinTerm bltE) body'
        -- * compiling match on a data/enum value
        Nothing -> do
          path <- qualR con
          -- report $ "  path: " <> ppR path
          (tel, _) <- telListView =<< A.typeOfConst con
          -- report $ " tel: " <> pp tel
          tel' <- populateArgNames tel
          -- report $ " tel': " <> pp tel'
          bodyCtx <- filterM (fmap not . isSrtOrLvlTy . snd . A.unDom) tel'
          -- report $ " bodyCtx: " <> pp bodyCtx
          let vTel = filter hasQuantityNon0 bodyCtx
          -- report $ " vTel: " <> pp vTel
          let xs = take n (fst . A.unDom <$> vTel)
          -- report $ " xs: " <> pp xs
          let pats = RId . R.mkIdent <$> xs
          -- report $ "  pat: " <> pp con <> "(" <> show n <> ")"
          --       <> " ~> " <> ppR path <> "(" <> intercalate "," (map ppR pats) <> ")"
          body'  <- A.addContext bodyCtx $ inLocalIntros (length xs) $ go body
          -- report $ "  body: " <> pp body <> " ~> " <> ppR body'
          body'' <- unboxPats con n xs body'
          -- report $ "  body: " <> pp body' <> " ~> " <> ppR body''
          return $ RArm (RTupleP path pats) body''
      where
      populateArgNames :: A.ListTel -> C (A.ListTel)
      populateArgNames [] = return []
      populateArgNames (d@(A.unDom -> (x, ty)):tel) = do
        x' <- freshVarInCtx x
        let d' = d {A.unDom = (x', ty)}
        (d' :) <$> A.addContext d' (populateArgNames tel)

      unboxPats :: A.QName -> Int -> [String] -> R.Expr () :~> R.Expr
      unboxPats con n xs e = inConstructor con $ do
        -- report $ "* unboxing " <> show xs
        ps <- flip mapMaybeM (enumerate0 xs) $ \(i, x) -> inArgument i $ do
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

