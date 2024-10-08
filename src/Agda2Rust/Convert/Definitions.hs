{-# LANGUAGE OverloadedStrings #-}
module Agda2Rust.Convert.Definitions (RDef(..)) where

import Control.Monad ( forM, when, filterM, (>=>) )
import Control.Arrow ( second )
import Data.Maybe ( isJust )

import Utils

import qualified Agda as A
import Agda.Lib ( TCM, liftTCM, mapMaybeM, (^.), funInline )
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
    -- ** ignore inlined functions (arising from instantiated modules)
    A.Function{..} | theDef ^. funInline -> True
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
      report $ " type: " <> pp defType
      goD pragma theDef
    where
    dx :: R.Ident
    dx = defNameR defn

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
        -- TODO: allow FFI for defined things

      -- ** postulates
      A.Axiom{..} -> do
        report " compiling postulate with `panic!`"
        (tel, resTy) <- telListView defType
        CompileDef <$> goFn pragma 0 (tel, resTy) \_ ->
          return $ rPanic "POSTULATE"

      -- ** type aliases
      A.Function{..}
        | isSortTerm (returnTy defType)
        , [A.Clause{..}] <- funClauses
        , Just t <- clauseBody
        , Nothing <- pragma
        -> inTyAlias $ do
        report " compiling type alias"
        params <- fmap rTyParam <$> mapMaybeM shouldKeepTyParam (A.telToList clauseTel)
        body   <- A.addContext clauseTel $ go (typeFromTerm t)
        return $ CompileDef $ RTyAlias' dx params body

      -- -- ** record projections
      -- A.Function{..} | Right proj <- funProjection -> do
      --   report $ " record projection: " <> pp (A.projOrig proj)
      --   return undefined

      -- ** functions
      A.Function{..} -> do
        report " compiling function"
        -- report $ " pragma: " <> show pragma
        when (any isNoFFI pragma) $
          setConst defName
        -- cc <- funCC defName
        -- report $ " cc: " <> pp cc
        tdef <- liftTCM $ A.toTreeless defName
        report $ " tdef: " <> pp tdef
        telV0 <- telListView defType
        -- report $ " telV0: " <> pp telV0
        let (topIntros, tterm) = A.tLamView tdef
        -- report $ " topIntros: " <> pp topIntros
        let droppedIntros = A.droppedPars defn
        -- report $ " droppedIntros: " <> pp droppedIntros
        telV <- telListViewUpTo (length (fst telV0)) defType
        -- report $ " telV: " <> pp telV
        CompileDef <$> goFn pragma (-topIntros) telV \raiseIntros -> do
          -- report $ " raiseIntros: " <> pp raiseIntros
          go (A.raise (raiseIntros - droppedIntros) tterm)
        where
      d | Just (NoFFI _) <- pragma ->
        panic "pragma (can only qualify pragma for functions)" (pp defName)

      -- ** datatypes
      A.Datatype{..} -> inDatatype defName $ do
        report " compiling datatype"
        -- cs <- zip dataCons <$> traverse typeOfConst dataCons)
        cs <- concat <$> forM dataCons (\dc -> do
          cDef <- A.instantiateDef =<< A.getConstInfo dc
          return [ (dc, A.defType cDef) | hasQuantityNon0 cDef]
          )
        -- report $ " cs: " <> pp cs
        A.TelV tel _ <- A.telViewUpTo dataPars defType
        -- report $ " tel: " <> pp tel
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
        report " compiling record"
        -- NB: incorporate conHead/namedCon in the future for accuracy
        --     + to solve the issue with private (non-public) fields
        -- report $ " recPars: " <> pp recPars
        -- report $ " recTel: " <> pp recTel
        -- report $ " recConHead: " <> pp recConHead
        setRecordConstructor recConHead
        let (tel, fs) = splitAt recPars (A.telToList recTel)
        -- report $ " tel: " <> pp tel
        -- report $ " fs: " <> pp fs
        params <- fmap R.mkIdent <$> mapMaybeM shouldKeepTyParam tel
        -- report $ " params: " <> show params
        fs' <- filterM shouldKeepRecField fs
        fields <- A.addContext tel $ goFs (A.unDom <$> fs')
        -- report $ " fields: " <> show fields
        let unusedParams = filter (unusedIn fields) params
        setUnusedTyParams defName (ppR <$> unusedParams)
        return $ CompileDef $ RStruct dx (RTyParam <$> params) (fields <>
          -- TODO: hygienic handling of hardcoded string "_phantom"
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
    goFn :: Maybe Pragma -> Int -> (A.ListTel, A.Type) -> (Int -> C (R.Expr ()))
         -> C (R.Item ())
    goFn pragma raiseIntros acc goBody = do
      (ps, as, resTy, body) <- goFn' 0 raiseIntros acc
      -- report $ "ps: " <> pp ps
      let mkFn | ConstNoFFI <- pragma{-, null as-} = RConstFn
               | isAnonymousName defName           = RPrivFn
               | otherwise                         = RFn
      if | ConstNoFFI <- pragma, null ps, null as
         -> return $ RConst dx resTy body
         | StaticNoFFI <- pragma, null ps, null as
         -> return $ RStatic dx resTy body
         | otherwise
         -> do setArity defName (length as)
               return $ mkFn dx (rTyParam <$> ps) (mkFnDeclTy as resTy) (RBlock body)
      where
      goFn' :: Int -> Int -> (A.ListTel, A.Type) -> C ([String], [R.Arg ()], R.Ty (), R.Expr ())
      goFn' allIntros raiseIntros (d:tel, resTy) = do
        -- report $ "_telArg: " <> pp d
        d' <- renameTelArg d
        -- report $ "_telArg': " <> pp d'
        (ps0, as0) <- goTelArg d'
        -- report $ "ps0: " <> pp ps0
        -- report $ "as0: " <> A.ppShow as0
        (ps, as, resTy, body) <- A.addContext d' $
          goFn' (allIntros + length as0) (raiseIntros + 1) (tel, resTy)
        return $ (ps0 <> ps, as0 <> as, resTy, body)
      goFn' allIntros raiseIntros ([], resTy) = do
        resTy' <- go resTy
        -- report $ " allIntros: " <> pp allIntros
        body <- inFunIntros allIntros $ goBody raiseIntros
        return ([], [], resTy', body)

      goTelArg :: TelItem -> C ([String], [R.Arg ()])
      goTelArg = classifyArg >=> \case
        TyParam x -> return ([x], [])
        LvlParam -> return ([], [])
        ErasedArg -> return ([], [])
        KeptArg x ty -> go ty >>= \ty' -> return ([], [RArg (R.mkIdent x) ty'])

      renameTelArg :: TelItem -> C TelItem
      renameTelArg d@(A.unDom -> (x, ty)) = do
        x' <- freshVarInCtx x
        return $ d {A.unDom = (x', ty)}
