{-# LANGUAGE OverloadedStrings #-}
module Agda2Rust.Convert.Definitions (RDef(..)) where

import Control.Monad ( forM, when )
import Control.Arrow ( second )
import Data.Maybe ( isJust )

import Utils

import qualified Agda as A
import Agda.Lib ( TCM, liftTCM, mapMaybeM )
import Agda.Utils
import Agda.Builtins ( isBuiltinDef )

import qualified Rust as R
import Rust.Utils

import Agda2Rust.Monad
import Agda2Rust.Pragma
import Agda2Rust.Convert.Class
import Agda2Rust.Convert.Names
import Agda2Rust.Convert.Terms
import Agda2Rust.Convert.Types
import Agda2Rust.Convert.Constructors

-- | Which Agda definitions to completely ignore?
ignoreDef :: A.Definition -> C Bool
ignoreDef d@A.Defn{..} = do
  isBltin <- liftTCM $ isBuiltinDef defName
  ignTy <- ignoreTy defType
  return $ case theDef of
    -- ** ignore builtin definitions
    _ | isBltin -> True
    -- ** ignore these types
    _ | ignTy -> True
    -- ** erasure
    _ | A.hasQuantity0 d -> True
    -- ** ignore functions that are @0/pattern-lambdas/with-generated
    A.Function{..} | {-funErasure ||-} isJust funExtLam || isJust funWith -> True
    -- ** ignored these kind of definitions
    A.Constructor{..} {-| conErasure-} -> True
    A.Primitive{..} -> True
    A.PrimitiveSort{..} -> True
    A.DataOrRecSig{..} -> True
    A.GeneralizableVar -> True
    -- ** compile everything else
    _ -> False
  where
  ignoreTy :: A.Type -> C Bool
  ignoreTy ty = do
    isLvl <- A.isLevelType =<< resTy ty
    return isLvl

-- | An Agda definition is either ignored or compiled to a Rust definition.
data RDef a = CompileDef (R.Item a) | IgnoreDef

-- | Compiling definitions.
instance A.Definition ~> RDef where
  go defn@A.Defn{..} = do
    shouldIgnore <- ignoreDef defn
    pragma <- liftTCM $ getRustPragma defName
    if shouldIgnore || (pragma == Just Ignore) then do
      report $ "\n*** ignoring definition: " <> pp defName
      return IgnoreDef
    else do
      report $ "\n*** compiling definition: " <> pp defName
      goD pragma theDef
    where
    dx :: R.Ident
    dx | Just (recName, recField) <- isRecordProjection theDef
       = unqualField recName recField
       | otherwise
       = unqualR defName

    goD :: Maybe Pragma -> A.Defn :~> RDef
    goD pragma = \case
      A.AbstractDefn defn -> goD pragma defn

      -- ** postulates (FFI)
      A.Axiom{..} | Just (FFI mq s) <- pragma -> do
        report " compiling postulate to user-provided FFI"
        -- report $ " -pragma: " <> s
        -- report $ " -pragma qualifier: " <> show mq
        setFFI defName (mq, s)
        return IgnoreDef

      d | Just (FFI _ _) <- pragma ->
        panic "pragma (can only register FFI for postulates)" (pp defName)

      -- ** postulates
      A.Axiom{..} -> do
        report $ " type: " <> pp defType
        report " compiling postulate with `panic!`"
        (tel, resTy) <- telListView defType
        CompileDef <$> goFn pragma (return $ rPanic "POSTULATE") (tel, resTy)

      -- ** type aliases
      A.Function{..}
        | isSortTy defType
        , [A.Clause{..}] <- funClauses
        , A.EmptyTel <- clauseTel
        , [] <- namedClausePats
        , Just t <- clauseBody
        -- , Nothing <- pragma
        -> CompileDef . RTyAlias dx <$> go (typeFromTerm t)

      -- -- ** record projections
      -- A.Function{..} | Right proj <- funProjection -> do
      --   report $ " record projection: " <> pp (A.projOrig proj)
      --   return undefined

      -- ** functions
      -- A.Function{..} | d ^. funInline
      A.Function{..} -> do
        report $ " type: " <> pp defType
        -- report $ " pragma: " <> show pragma
        when (any isNoFFI pragma) $
          setConst defName
        tdef <- liftTCM $ A.toTreeless defName
        report $ " tdef: " <> pp tdef
        let (tterm, introVars) = stripTopTLams tdef
        -- report $ " tterm: " <> pp tterm
        -- report $ " intros: " <> pp introVars
        (tel0, _) <- telListView defType
        intros <- calculateIntros introVars tel0
        (tel, resTy) <- telListViewUpTo intros defType
        -- report $ " >>tel: " <> pp tel
        -- report $ " >>resTy: " <> pp resTy
        -- let ctx | Right A.Projection{..} <- funProjection
        --         , Just recName <- projProper
        --         = RMod (unqualR recName)
        --         | otherwise
        --         = id
        -- ctx <$> goFn (go tterm) (tel, resTy)
        CompileDef <$> goFn pragma (go tterm) (tel, resTy)
        where
        stripTopTLams :: A.TTerm -> (A.TTerm, Int)
        stripTopTLams = \case
          A.TLam t -> second (+ 1) (stripTopTLams t)
          t        -> (t, 0)

        calculateIntros :: Int -> A.ListTel -> C Int
        calculateIntros _ tel = return (length tel)
        -- calculateIntros = go 0
        --   where
        --   go n vIntros (d@(A.unDom -> (x, ty)) : tel) = do
        --     isSrt <- isSortResTy ty
        --     isLvl <- A.isLevelType ty
        --     let shouldDrop = isLvl || A.hasQuantity0 d
        --         isNotV = isSrt || shouldDrop
        --     if isNotV then
        --       A.addContext d $ go (n + 1) vIntros tel
        --     else if vIntros == 0 then
        --       return n
        --     else
        --       A.addContext d $ go (n + 1) (vIntros - 1) tel
        --   go n _ [] = return n

      d | Just (NoFFI _) <- pragma ->
        panic "pragma (can only qualify pragma for functions)" (pp defName)

      -- ** datatypes
      A.Datatype{..} -> inDatatype defName $ do
        -- cs <- zip dataCons <$> traverse typeOfConst dataCons)
        cs <- concat <$> forM dataCons (\dc -> do
          cDef <- A.instantiateDef =<< A.getConstInfo dc
          return [ (dc, A.defType cDef) | hasQuantityNon0 cDef]
          )
        report $ " cs: " <> pp cs
        A.TelV tel _ <- A.telViewUpTo dataPars defType
        report $ " tel: " <> pp tel
        params <- fmap R.mkIdent <$> mapMaybeM shouldKeepTyParam (A.telToList tel)
        variants <- A.addContext tel (gos cs)
        let unusedParams = filter (unusedIn variants) params
        setUnusedTyParams defName (ppR <$> unusedParams)
        return $ CompileDef $ REnum dx (RTyParam <$> params) (variants <>
          [ RVariant "_Impossible" [ RField $ phantomField unusedParams ]
          | not (null unusedParams)
          ])

      -- ** records
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
        params <- fmap R.mkIdent <$> mapMaybeM shouldKeepTyParam tel
        report $ " params: " <> show params
        fields <- A.addContext tel (goFs $ A.unDom <$> filter hasQuantityNon0 fs)
        report $ " fields: " <> show fields
        let unusedParams = filter (unusedIn fields) params
        setUnusedTyParams defName (ppR <$> unusedParams)
        return $ CompileDef $ RStruct dx (RTyParam <$> params) (fields <>
          [ RNamedField "_phantom" (phantomField unusedParams)
          | not (null unusedParams)
          ])


        where
        goFs :: [(String, A.Type)] :~>* R.StructField
        goFs [] = return []
        goFs ((x, ty):fs) =
          (:) <$> goF (x, ty)
              <*> A.addContext (A.defaultDom (x, ty)) (goFs fs)

        goF :: (String, A.Type) :~> R.StructField
        goF (x, ty) = RNamedField (R.mkIdent $ transcribe x) <$> go ty

      d -> panic "definition" d

    -- ** helper for functions and axioms
    goFn :: Maybe Pragma -> C (R.Expr ()) -> (A.ListTel, A.Type) -> C (R.Item ())
    goFn pragma goBody acc = do
      (ps, as, resTy, body) <- goFn' acc
      let mkFn | ConstNoFFI <- pragma{-, null as-} = RConstFn
               | otherwise = RFn
      return $ if
        | ConstNoFFI <- pragma, null ps, null as
        -> RConst dx resTy body
        | StaticNoFFI <- pragma, null ps, null as
        -> RStatic dx resTy body
        | otherwise
        -> mkFn dx (RTyParam . R.mkIdent <$> ps) (RFnTy as resTy) (RBlock body)
      where
      goFn' :: (A.ListTel, A.Type) -> C ([String], [R.Arg ()], R.Ty (), R.Expr ())
      goFn' (d:tel, resTy) = do
        -- report $ "_telArg: " <> pp d
        d' <- renameTelArg d
        -- report $ "_telArg': " <> pp d'
        (ps0, as0) <- goTelArg d'
        (ps, as, resTy, body) <- A.addContext d' (goFn' (tel, resTy))
        return $ (ps0 <> ps, as0 <> as, resTy, body)
      goFn' ([], resTy) = do
        resTy' <- go resTy
        body <- goBody
        return ([], [], resTy', body)

      goTelArg :: A.Dom (String, A.Type) -> C ([String], [R.Arg ()])
      goTelArg d@(A.unDom -> (x, ty)) = do
        -- let isSrt = isSortTy ty
        isSrt <- isSortResTy ty
        isLvl <- A.isLevelType ty
        let shouldDrop = isLvl || A.hasQuantity0 d
        if isSrt then
          return ([x], [])
        else do
          ty' <- go ty
          return ([], [RArg (R.mkIdent x) ty' | not shouldDrop])

      renameTelArg :: A.Dom (String, A.Type) -> C (A.Dom (String, A.Type))
      renameTelArg d@(A.unDom -> (x, ty))
        | "_" <- x
        = do x' <- freshVarInCtx
             return $ d {A.unDom = (x', ty)}
        | otherwise
        = return d
