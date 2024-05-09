{-# LANGUAGE FlexibleInstances #-}
module Agda2Rust.Convert.Types () where

import Control.Monad ( when, filterM )
import Control.Monad.Reader ( asks )

import Utils

import qualified Agda as A
import Agda.Lib ( ifJustM )
import Agda.Utils

import qualified Rust as R
import Rust.Utils

import Agda2Rust.Monad
import Agda2Rust.Convert.Class
import Agda2Rust.Convert.Names
import Agda2Rust.Convert.Builtins
import Agda2Rust.Convert.FFI ( compileFFITy )

-- | Compiling types.
instance A.Type ~> R.Ty where
  go ty = asks curDatatype >>= \curD -> do
    report $ " ** compiling type: " <> pp ty
    -- typeT <- liftTCM $ A.closedTermToTreeless A.LazyEvaluation (unEl ty)
    -- report $ " type (treeless): " <> pp typeT
    case A.unEl ty of
      -- ** defined types
      A.Def n es | es <- A.argsFromElims es ->
        ifJustM (isBuiltinTy n) (compileBuiltinTy ty es) $
        ifJustM (getFFI n)      (compileFFITy ty es)     $ do
          -- report $ "  es: " <> pp es
          dTy <- A.typeOfConst n
          -- report $ "  >> dTy: " <> pp dTy
          as <- flip filterM (enumerate0 es) \(i, a) -> do
            eTy <- getArgTy dTy i
            isLvl <- A.isLevelType eTy
            -- report $ "  >> eTy: " <> pp eTy
            -- let isSrt = isSortTy eTy
            isSrt <- isSortResTy eTy
            return $ hasQuantityNon0 a && not isLvl && isSrt
          -- report $ "  as: " <> pp as
          let toBox = curD == Just n
          when toBox setBox
          (if toBox then RBoxTy else id) . rTyRef' (unqualR n)
            <$> gos (typeFromTerm . A.unArg . snd <$> as)

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
        ctx <- currentCtx
        -- report $ "  ctx: " <> pp ctx
        x <- lookupCtxVar i
        -- report $ "  ctx[" <> pp i <> "]: " <> pp x
        -- es' <- traverse go (vArgs es)
        return $ RTyRef (R.mkIdent x)

      -- ** type lambdas
      -- TODO: currently only supports const lambdas `λ _ -> ...`
      A.Lam _ (A.Abs x ty) ->
        A.addContext [(x, defaultTy)] $ go (typeFromTerm ty)

      -- otherwise, error
      ty -> panic "type" ty
