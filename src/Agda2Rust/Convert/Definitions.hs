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
        -- TODO: allow FFI for defined things

      -- ** postulates
      A.Axiom{..} -> do
        report " compiling postulate with `panic!`"
        (tel, resTy) <- telListView defType
        CompileDef <$> goFn pragma 0 (\_ -> return $ rPanic "POSTULATE") (tel, resTy)

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
        report $ " telV0: " <> pp telV0
        let (intros, tterm) = A.tLamView tdef
        report $ " intros: " <> pp intros
        -- telV <- telListViewUpTo (length (fst telV0) - intros) defType
        telV <- telListViewUpTo (length (fst telV0)) defType
        report $ " telV: " <> pp telV
        CompileDef <$> goFn pragma 0 (\intros -> {-inFunIntros intros $-} go tterm) telV
{-
        let (intros, tterm) = A.tLamView tdef
        -- report $ " intros: " <> pp intros
        -- report $ " tterm: " <> pp tterm
        telV@(tel, _) <- telListView defType
        -- report $ " tel: " <> pp tel
        ctel <- classifyArgs tel
        let telIntros = length (notTyParams ctel)
        -- report $ " telIntros: " <> pp telIntros
        let newIntros = telIntros - intros
        -- report $ " newIntros: " <> pp newIntros
        CompileDef <$> goFn pragma intros (\_ -> go $ A.raise 0 tterm) telV
-}
{-
        report $ " intros: " <> pp intros
        telV@(tel, resTy) <- telListView defType
        report $ " tel: " <> pp tel
        -- report $ " resTy: " <> pp resTy

        -- intros <- calculateIntros introVars tel0
        -- (tel, resTy) <- telListViewUpTo intros defType

        -- telIntros <- length <$> filterM (fmap not . isTyParamM) tel
        -- let telIntros = length $ filter hasQuantityNon0 tel
        -- let telIntros = length tel
        tel1 <- shouldKeepTel tel
        report $ " tel1: " <> pp tel1
        let telIntros = length tel1
        let newIntros = telIntros - intros
        report $ " newIntros: " <> pp newIntros
        when (newIntros < 0) $ error "newIntros is negative!"

        let tterm' = A.raise newIntros tterm
        -- let tterm' = tterm
        report $ " tterm': " <> pp tterm'
        -- let (tterm', _) = stripTopTLams $ etaExpandT newIntros tterm

        CompileDef <$> goFn pragma (inFunIntros newIntros $ go tterm') telV
-}
        where
        -- calculateIntros :: Int -> A.ListTel -> C Int
        -- calculateIntros _ tel = return (length tel)
        -- calculateIntros n _ = return n
        -- calculateIntros = go 0
        --   where
        --   go :: Int -> Int -> A.ListTel -> C Int
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
        report " compiling datatype"
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
        report " compiling record"
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
    goFn :: Maybe Pragma -> Int -> (Int -> C (R.Expr ())) -> (A.ListTel, A.Type) -> C (R.Item ())
    goFn pragma intros goBody acc = do
      (ps, as, resTy, body) <- goFn' 0 acc
      report $ "ps: " <> pp ps
      let mkFn | ConstNoFFI <- pragma{-, null as-} = RConstFn
               | otherwise = RFn
      if | ConstNoFFI <- pragma, null ps, null as
         -> return $ RConst dx resTy body
         | StaticNoFFI <- pragma, null ps, null as
         -> return $ RStatic dx resTy body
         | otherwise
         -> do setArity defName (length as)
               return $ mkFn dx (rTyParam <$> ps) (mkFnDeclTy as resTy) (RBlock body)
      where
      goFn' :: Int -> (A.ListTel, A.Type) -> C ([String], [R.Arg ()], R.Ty (), R.Expr ())
      goFn' allIntros (d:tel, resTy) = do
        -- report $ "_telArg: " <> pp d
        d' <- renameTelArg d
        -- report $ "_telArg': " <> pp d'
        (ps0, as0) <- goTelArg d'
        -- report $ "ps0: " <> pp ps0
        -- report $ "as0: " <> A.ppShow as0
        (ps, as, resTy, body) <- A.addContext d' (goFn' (allIntros + length as0) (tel, resTy))
        return $ (ps0 <> ps, as0 <> as, resTy, body)
      goFn' allIntros ([], resTy) = do
        resTy' <- go resTy
        {-
        let newIntros = max (allIntros - intros) 0
        body <- inFunIntros newIntros $ goBody newIntros
        -}
        body <- goBody allIntros
        return ([], [], resTy', body)

      goTelArg :: TelItem -> C ([String], [R.Arg ()])
      goTelArg = classifyArg >=> \case
        TyParam x -> return ([x], [])
        DroppedArg -> return ([], [])
        KeptArg x ty -> go ty >>= \ty' -> return ([], [RArg (R.mkIdent x) ty'])

      renameTelArg :: TelItem -> C TelItem
      renameTelArg d@(A.unDom -> (x, ty)) = do
        x' <- freshVarInCtx x
        return $ d {A.unDom = (x', ty)}
