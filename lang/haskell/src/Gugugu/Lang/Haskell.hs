{-|
Haskell target
 -}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module Gugugu.Lang.Haskell
  ( GuguguHaskellOption(..)
  , makeFiles
  ) where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Bifunctor
import           Data.Char
import           Data.Foldable
import qualified Data.List                       as List
import           Data.List.NonEmpty              (NonEmpty (..))
import qualified Data.List.NonEmpty              as NonEmpty
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import           Data.Set                        (Set)
import qualified Data.Set                        as Set
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Data.Traversable
import           System.FilePath
import           Text.Printf

import           Gugugu.Resolver
import           Gugugu.Utilities

import           Gugugu.Lang.Haskell.SourceUtils


-- | Option for 'makeFiles'
data GuguguHaskellOption
  = GuguguHaskellOption
    { packagePrefix    :: [Text]                  -- ^ Package prefix
    , runtimeMod       :: [Text]                  -- ^ Runtime module
    , derivings        :: [Text]                  -- ^ derivings clause
    , withCodec        :: Bool                    -- ^ True if generate codec
    , withServer       :: Bool                    -- ^ True if generate server
    , withClient       :: Bool                    -- ^ True if generate client
    , nameTransformers :: GuguguNameTransformers  -- ^ Name transformers
    }
  deriving Show

-- | Make Haskell AST from 'Module's
makeFiles :: MonadError String m
          => GuguguHaskellOption
          -> [Module]
          -> m (Map FilePath HaskellModule)
makeFiles = makeModules


type GuguguK r m = ( MonadError String m
                   , MonadReader r m
                   , HasGuguguHaskellOption r
                   , HasResolutionContext r
                   )

data GuguguHaskellEnv
  = GuguguHaskellEnv
    { gOpts :: GuguguHaskellOption
    , gCtx  :: ResolutionContext
    }
  deriving Show

class HasGuguguHaskellOption a where
  toGuguguHaskellOption :: a -> GuguguHaskellOption

type ModuleItems = (Set ImportDecl, [TopDecl])
type ForeignItems = (ModuleItems, ([Decl], [Decl]))

makeModules :: MonadError String m
            => GuguguHaskellOption
            -> [Module]
            -> m (Map FilePath HaskellModule)
makeModules opts@GuguguHaskellOption{..} modules = do
  let moduleMap = Map.fromList $
        fmap (\md@Module{..} -> (moduleName, md)) modules
  foreignAndFiles <- for modules $ \md -> do
    let rCtx = ResolutionContext
          { rcModules       = moduleMap
          , rcCurrentModule = md
          }
        env  = GuguguHaskellEnv
          { gOpts = opts
          , gCtx  = rCtx
          }
    runReaderT (makeModule md) env
  let (foreigns, files) = unzip foreignAndFiles
  let ((foreignImports, foreignTops), (foreignEncodes, foreignDecodes)) =
        fold foreigns
  let allFiles     = Map.fromList $
           files
        <> [(codecPath, codecModule) | withCodec]
        <> [(transPath, transModule) | withServer || withClient]
      codecModule  = HaskellModule
        { hmExts    = ["FunctionalDependencies", "MultiParamTypeClasses"]
        , hmId      = codecModId
        , hmImports = Set.toAscList $
               unsafeImportMods' ["Data.Int", "Data.Text", "Data.Vector"]
            <> foreignImports
        , hmDecls   =
            [ fClass "ForeignEncodersImpl" foreignEncodes
            , fClass "ForeignDecodersImpl" foreignDecodes
            ] <> foreignTops
        }
        where
          fClass n ds = TdClass ClassDecl
            { cdName  = n
            , cdTVars = ["c", "f"]
            , cdDecls = ds
            }
      transModule  = HaskellModule
        { hmExts    = ["ExistentialQuantification", "RankNTypes"]
        , hmId      = transModId
        , hmImports = Set.toAscList $
            (if withClient then importMods' codecModId else Set.empty) <>
            unsafeImportMods' ["Data.Text", "Data.Vector"]
        , hmDecls   = [guguguClient | withClient]
        }
      guguguClient = TdData DataDecl
        { ddName      = "GuguguClient"
        , ddTvs       = transParams
        , ddCons      = [gcCon]
        , ddDerivings = []
        }
        where gcCon    = CEQ
                ["ca", "cb", "fa", "fb"]
                [ codecType "a" $ TQCon $ codecV "EncoderImpl"
                , codecType "b" $ TQCon $ codecV "DecoderImpl"
                ]
                "MkGuguguClient"
                [ TSimple "ca"
                , TSimple "cb"
                , TParen $ transType $ TSimple "ClientTransport"
                ]
      codecV n     = codecModId <> (n :| [])
      codecPath    = modulePath codecModId
      transPath    = modulePath transModId
      codecModId   = NonEmpty.fromList $ runtimeMod <> ["Codec"]
      transModId   = NonEmpty.fromList $ runtimeMod <> ["Transport"]
  pure allFiles

makeModule :: GuguguK r m
           => Module
           -> m (ForeignItems, (FilePath, HaskellModule))
makeModule md@Module{..} = do
  GuguguHaskellOption{..} <- asks toGuguguHaskellOption
  importsAndDecs <- traverse makeData moduleDatas
  importsAndInstDecs <- if withCodec
    then traverse (makeCodecInsts md) moduleDatas else pure mempty
  (imports'', transports) <-
    if (withServer || withClient) && not (null moduleFuncs)
    then makeTransports md else pure mempty
  mod' <- mkModuleCode md
  let (imports, decs) = fold importsAndDecs
  let (foreignItems, (imports', instDecs)) = fold importsAndInstDecs
  let moduleBody = HaskellModule
        { hmExts    = List.sort $
               ["OverloadedStrings" | withCodec]
            <> ( if hasTrans
                  then ["OverloadedLists", "MultiParamTypeClasses"]
                  else []
               )
            <> ["FlexibleInstances" | withClient && hasTrans]
        , hmId      = mod'
        , hmImports = Set.toAscList $ imports <> imports' <> imports''
        , hmDecls   = decs <> transports <> instDecs
        }
      path       = modulePath mod'
      hasTrans   = not $ null transports
  pure (foreignItems, (path, moduleBody))

makeData :: GuguguK r m
         => Data
         -> m (Set ImportDecl, [TopDecl])
makeData d@Data{..} = do
  GuguguHaskellOption{..} <- asks toGuguguHaskellOption
  dataCode <- mkTypeCode d
  (imports, dec) <- case dataConDef of
    Just (DRecord RecordCon{..}) -> do
      importAndFields <- for recordConFields $ \rf@RecordField{..} -> do
        fieldCode <- mkFieldCode d rf
        (iField, hsType) <- makeType recordFieldType
        pure (iField, (fieldCode, hsType))
      let dataDec = TdData DataDecl
            { ddName      = dataCode
            , ddTvs       = []
            , ddCons      = [CRecord dataCode fields]
            , ddDerivings = fmap qSimple derivings
            }
          (iFields, fields) = unzip importAndFields
      pure (fold iFields, dataDec)
    Just (DEnum names)           -> do
      constrs <- for names $ \name -> do
        enumCode <- mkEnumCode name
        pure $ Constr enumCode
      let dataDec = TdData DataDecl
            { ddName      = dataCode
            , ddTvs       = []
            , ddCons      = toList constrs
            , ddDerivings = fmap qSimple derivings
            }
      pure (Set.empty, dataDec)
    Nothing                      -> do
      (imports, foreignTyCon) <- resolveForeign' d
      let typeDec = TdType TypeDecl
            { tdName = dataCode
            , tdType = TQCon foreignTyCon
            }
      pure (imports, typeDec)
  pure (imports, [dec])

makeCodecInsts :: GuguguK r m
               => Module
               -> Data
               -> m (ForeignItems, ModuleItems)
makeCodecInsts md d@Data{..} = do
  GuguguHaskellOption{..} <- asks toGuguguHaskellOption
  case dataConDef of
    Just dataCon -> do
      let codecV n = codecId <> (n :| [])
          codecId  = NonEmpty.fromList $ runtimeMod <> ["Codec"]
      dataCode <- mkTypeCode d
      (encodeDef, decodeDef) <- case dataCon of
        DRecord RecordCon{..} -> do
          let eC = ESimple "c"
          codecComps <- for (indexed recordConFields) $ \(i, rf) -> do
            fieldValue <- mkFieldValue rf
            let encodeExpr = foldl' EApp (EQual $ codecV "encodeRecordField")
                  [eI, fieldValue, eC, ESimple vI]
                decodeExpr = foldl' EApp (EQual $ codecV "decodeRecordField")
                  [eI, fieldValue, eC]
                eI         = ESimple $ showText i
                vI         = "v" <> showText i
            pure (vI, encodeExpr, decodeExpr)
          let (vs, encodeExprs, decodeExprs) = unzip3 codecComps
          let encodeDef = DDef Def
                { dLhs    = "encode"
                , dParams = []
                , dRhs    = ELet
                    [ DDef Def
                        { dLhs    = "go"
                        , dParams = ["c", "a"]
                        , dRhs    = ECase (ESimple "a")
                            [(PCon (qSimple dataCode) vs, encodeValue)]
                        }
                    ] $ EQual (codecV "encodeRecord")
                          `EApp` nFields `EApp` ESimple "go"
                }
                where encodeValue = case encodeExprs of
                        []     -> ESimple "pure" `EApp` ESimple "()"
                        e : es -> foldl' (\z e' -> EBinary z "*>" e') e es
              decodeDef = DDef Def
                { dLhs    = "decode"
                , dParams = []
                , dRhs    = ELet
                    [ DDef Def
                        { dLhs    = "go"
                        , dParams = ["c"]
                        , dRhs    = decodeValue
                        }
                    ] $ EQual (codecV "decodeRecord")
                          `EApp` nFields `EApp` ESimple "go"
                }
                where decodeValue = case decodeExprs of
                        []     -> ESimple "pure" `EApp` hCon
                        e : es -> foldl' (\z e' -> EBinary z "<*>" e')
                                    (EBinary hCon "<$>" e) es
                      hCon        = ESimple dataCode
              nFields   = ESimple $ showText $ length recordConFields
          pure (encodeDef, decodeDef)
        DEnum names           -> do
          codecCases <- for (indexed $ toList names) $ \(i, name) -> do
            enumCode <- mkEnumCode name
            enumValue <- mkEnumValue name
            let encodeCase = (PSimple enumCode, (ESimple cI, ESimple enumValue))
                decodeCase = ((PSimple cI, PSimple enumValue), decoded)
                decoded    = ESimple "Just" `EApp` ESimple enumCode
                cI         = showText i
            pure (encodeCase, decodeCase)
          let (encodeCases, decodeCases) = unzip codecCases
          let encodeDef    = DDef Def
                { dLhs    = "encode"
                , dParams = []
                , dRhs    = ELet
                    [def' "asIndex" fst, def' "asName" snd] $
                    EQual (codecV "encodeEnum")
                      `EApp` ESimple "asIndex" `EApp` ESimple "asName"
                }
                where def' name selector = DDef Def
                        { dLhs    = name
                        , dParams = ["v"]
                        , dRhs    = ECase (ESimple "v") $
                            fmap (second selector) encodeCases
                        }
              decodeDef    = DDef Def
                { dLhs    = "decode"
                , dParams = []
                , dRhs    = ELet
                    [def' "byIndex" "i" fst, def' "byName" "n" snd] $
                    EQual (codecV "decodeEnum")
                      `EApp` ESimple "byIndex" `EApp` ESimple "byName"
                }
                where def' name arg selector = DDef Def
                        { dLhs    = name
                        , dParams = [arg]
                        , dRhs    = ECase (ESimple arg) $
                               fmap (first selector) decodeCases
                            <> [(PSimple "_", ESimple "Nothing")]
                        }
          pure (encodeDef, decodeDef)
      let allDecs      = [encodingDecl, decodingDecl]
          encodingDecl = TdInst InstDecl
            { idClass = codecV "Encoding"
            , idTypes = [thisType]
            , idDecls = [encodeDef]
            }
          decodingDecl = TdInst InstDecl
            { idClass = codecV "Decoding"
            , idTypes = [thisType]
            , idDecls = [decodeDef]
            }
          thisType     = TSimple dataCode
          imports      = importMods' codecId
      pure (mempty, (imports, allDecs))
    Nothing      -> do
      (imports, hsId) <- resolveForeign' d
      (fEncodeName, fDecodeName) <- mkForeignCodecName md d
      let allDecs      = [encodingDecl, decodingDecl]
          encodingDecl = TdInst InstDecl
            { idClass = qSimple "Encoding"
            , idTypes = [TQCon hsId]
            , idDecls = [ DDef Def
                            { dLhs    = "encode"
                            , dParams = []
                            , dRhs    = ESimple fEncodeName
                            }
                        ]
            }
          decodingDecl = TdInst InstDecl
            { idClass = qSimple "Decoding"
            , idTypes = [TQCon hsId]
            , idDecls = [ DDef Def
                            { dLhs    = "decode"
                            , dParams = []
                            , dRhs    = ESimple fDecodeName
                            }
                        ]
            }
          fEncodeDec   = DTS TypeSig
            { tsVar  = fEncodeName
            , tsType = TSimple "c" `TArrow` thisType `TArrow` tF (TSimple "()")
            }
          fDecodeDec   = DTS TypeSig
            { tsVar  = fDecodeName
            , tsType = TSimple "c" `TArrow` tF thisType
            }
          thisType     = TQCon hsId
          tF t         = TSimple "f" `TApp` t
      pure (((imports, allDecs), ([fEncodeDec], [fDecodeDec])), mempty)

makeTransports :: GuguguK r m
               => Module
               -> m ModuleItems
makeTransports md@Module{..} = do
  GuguguHaskellOption{..} <- asks toGuguguHaskellOption
  let codecV n   = codecId <> (n :| [])
      codecId    = NonEmpty.fromList $ runtimeMod <> ["Codec"]
      transV n   = transId <> (n :| [])
      transId    = NonEmpty.fromList $ runtimeMod <> ["Transport"]
      eCodec n c = EParen $ EQual (codecV n) `EApp` ESimple c
      nNamespace = "_namespace"
      eNamespace = ESimple nNamespace
  transportComps <- for moduleFuncs $ \fn@Func{..} -> do
    (fdImports, fd) <- makeType funcDomain
    (fcdImports, fcd) <- case funcCodomain of
      GApp{ typeCon = "IO", typeParams = [v] } -> makeType v
      _                                        ->
        throwError "Function codomain must be a type like IO a"
    funcCode <- mkFuncCode fn
    funcValue <- mkFuncValue fn
    let funcSig   = DTS TypeSig
          { tsVar  = funcCode
          , tsType = TSimple "a" `TArrow`
                     (TSimple "f" `TApp` fd) `TArrow`
                     (TSimple "m" `TApp` TParen (TSimple "g" `TApp` fcd))
          }
        serverAlt = (PSimple funcValue, ESimple "Just" `EApp` serverExp)
        clientDef = DDef Def
          { dLhs    = funcCode
          , dParams = ["c"]
          , dRhs    = ECase (ESimple "c")
              [(PCon (transV "MkGuguguClient") ["ca", "cb", "k"], clientExp)]
          }
        serverExp = EParen $ ESimple "k"
          `EApp` eCodec "decodeValue" "ca"
          `EApp` eCodec "encodeValue" "cb"
          `EApp` EParen (ESimple funcCode `EApp` ESimple "a")
        clientExp = ESimple "k"
          `EApp` eCodec "encodeValue" "ca"
          `EApp` eCodec "decodeValue" "cb"
          `EApp` EParen eQn
          where eQn = EQual (transV "QualName")
                  `EApp` eNamespace `EApp` ESimple funcValue
    pure (fdImports <> fcdImports, funcSig, serverAlt, clientDef)
  className <- mkModuleType md "Module"
  transportName <- mkModuleType md "Transport"
  moduleValue <- mkModuleValue md
  let (funcImports, classFuncs, serverAlts, clientDecs) =
        List.unzip4 transportComps
  let allDecls   = [classDec]
                <> (if withServer then allServers else [])
                <> [nsSig, nsDef]
                <> [clientInst | withClient]
      classDec   = TdClass ClassDecl
        { cdName  = className
        , cdTVars = tParams
        , cdDecls = classFuncs
        }
      allServers = [serverSig, serverDef]
      serverSig  = TdSig TypeSig
        { tsVar = serverDefN
        , tsType = TConstrained
            [ codecType "a" $ TQCon $ codecV "DecoderImpl"
            , codecType "b" $ TQCon $ codecV "EncoderImpl"
            , tApps' (TSimple className) tParams
            ] $ TSimple "ca" `TArrow`
                TSimple "cb" `TArrow`
                TSimple "a" `TArrow`
                transType (TQCon $ transV "ServerTransport")
        }
      serverDef  = TdDef Def
        { dLhs    = serverDefN
        , dParams = ["ca", "cb", "a", "qn", "k"]
        , dRhs    = ECase (ESimple "qn")
            [(PCon (transV "QualName") ["ns", "n"], e1)]
        }
        where
          e1 = ECase (EBinary (ESimple "ns") "==" eNamespace)
            [ (PSimple "False", ESimple "Nothing")
            , (PSimple "True", e2)
            ]
          e2 = ECase (ESimple "n") $
            serverAlts <> [(PSimple "_", ESimple "Nothing")]
      serverDefN = "mk" <> transportName
      clientInst = TdInst InstDecl
        { idClass = qSimple className
        , idTypes =
            [ TParen $ transType $ TQCon $ transV "GuguguClient"
            , TSimple "f", TSimple "g", TSimple "m"
            ]
        , idDecls = clientDecs
        }
      nsSig      = TdSig TypeSig
        { tsVar = nNamespace
        , tsType = unsafeTCon "Data.Vector.Vector"
            `TApp` unsafeTCon "Data.Text.Text"
        }
      nsDef      = TdDef Def
        { dLhs    = nNamespace
        , dParams = []
        , dRhs    = moduleValue
        }
      tParams    = ["a", "f", "g", "m"]
      allImports = fold funcImports
        <> unsafeImportMods' ["Data.Vector", "Data.Text"]
        <> importMods [codecId, transId]
  pure (allImports, allDecls)


makeType :: GuguguK r m => GType -> m (Set ImportDecl, Type)
makeType GApp{..} = do
  (imports, tFirst) <- resolveHaskellType typeCon
  importsAndParams <- traverse makeType typeParams
  let hsType             = foldl' f (TQCon tFirst) params
      f z t              = case t of
        TApp _ _   -> z `TApp` TParen t
        TArrow _ _ -> z `TApp` TParen t
        _          -> z `TApp` t
      allImports         = fold imports' <> imports
      (imports', params) = unzip importsAndParams
  pure (allImports, hsType)

resolveHaskellType :: GuguguK r m => Text -> m (Set ImportDecl, QualId)
resolveHaskellType t = do
  rr <- resolveTypeCon t
  case rr of
    ResolutionError e -> throwError e
    LocalType d       -> do
      typeName <- mkTypeCode d
      pure (Set.empty, typeName :| [])
    Imported md d     -> do
      modName <- mkModuleCode md
      typeName <- mkTypeCode d
      let hsType  = modName <> (typeName :| [])
          imports = importMods' modName
      pure (imports, hsType)
    Primitive pt      -> do
      let simple      t' = (Set.empty, t' :| [])
          imported ns t' =
            let qn      = unsafeQ ns
                imports = importMods' qn
            in (imports, qn <> (t' :| []))
      pure $ case pt of
        PUnit   -> simple "()"
        PBool   -> simple "Bool"
        PInt32  -> imported "Data.Int" "Int32"
        PDouble -> simple "Double"
        PString -> imported "Data.Text" "Text"
        PMaybe  -> simple "Maybe"
        PList   -> imported "Data.Vector" "Vector"

resolveForeign' :: GuguguK r m => Data -> m (Set ImportDecl, QualId)
resolveForeign' Data{..} = do
  foreignPragma <- case Map.lookup thisTarget dataForeignMap of
    Just v  -> pure v
    Nothing -> throwError $ printf
      "Type %s does not have foreign pragma" dataName
  case NonEmpty.nonEmpty $ splitOn' "." foreignPragma of
    Nothing -> throwError $ printf "Bad FOREIGN pragma: %s" foreignPragma
    Just qn -> do
      let imports = case NonEmpty.nonEmpty $ NonEmpty.init qn of
            Just modName -> importMods' modName
            Nothing      -> Set.empty
      pure (imports, qn)

modulePath :: QualId -> FilePath
modulePath (part1 :| parts) =
  foldl' (\z t -> z </> T.unpack t) (T.unpack part1) parts <.> "hs"


-- Name transformers

mkModuleCode :: GuguguK r m => Module -> m QualId
mkModuleCode Module{..} = do
  GuguguHaskellOption{..} <- asks toGuguguHaskellOption
  withTransformer transModuleCode $ \f ->
    NonEmpty.fromList $ packagePrefix <> [f moduleName]

mkModuleValue :: GuguguK r m => Module -> m Exp
mkModuleValue Module{..} = withTransformer transModuleValue $ \f ->
  EList [ESimple $ unsafeQuote $ f moduleName]

mkModuleType :: GuguguK r m => Module -> Text -> m Text
mkModuleType Module{..} suffix = withTransformer transModuleType $ \f ->
  f $ moduleName <> suffix

mkFuncCode :: GuguguK r m => Func -> m Text
mkFuncCode Func{..} = withTransformer transFuncCode $ \f ->
  f funcName

mkFuncValue :: GuguguK r m => Func -> m Text
mkFuncValue Func{..} = withTransformer transFuncValue $ \f ->
  unsafeQuote $ f funcName

mkTypeCode :: GuguguK r m => Data -> m Text
mkTypeCode Data{..} = withTransformer transTypeCode $ \f ->
  f dataName

mkTypeFunc :: GuguguK r m => Text -> m Text
mkTypeFunc name = withTransformer transTypeFunc $ \f ->
  f name

mkFieldCode :: GuguguK r m => Data -> RecordField -> m Text
mkFieldCode Data{..} RecordField{..} = withTransformer transFieldCode $ \f ->
  let withFirst g t = case T.uncons t of
        Nothing      -> t
        Just (c, t') -> T.cons (g c) t'
  in f $ withFirst toLower dataName <> withFirst toUpper recordFieldName

mkFieldValue :: GuguguK r m => RecordField -> m Exp
mkFieldValue RecordField{..} = withTransformer transFieldValue $ \f ->
  ESimple $ unsafeQuote $ f recordFieldName

mkEnumCode :: GuguguK r m => Text -> m Text
mkEnumCode name = withTransformer transEnumCode $ \f ->
  f name

mkEnumValue :: GuguguK r m => Text -> m Text
mkEnumValue name = withTransformer transEnumValue $ \f ->
  unsafeQuote $ f name


-- Utilities

thisTarget :: Text
thisTarget = "haskell"

qSimple :: Id -> QualId
qSimple t = t :| []

unsafeQ :: Text -> QualId
unsafeQ = NonEmpty.fromList . splitOn' "."

unsafeTCon :: Text -> Type
unsafeTCon = TQCon . unsafeQ

tApps' :: Type -> [Id] -> Type
tApps' = foldl' (\z -> TApp z . TSimple)

importMod' :: QualId -> ImportDecl
importMod' qid = ImportDecl{ idModuleId = qid }

importMods :: [QualId] -> Set ImportDecl
importMods = Set.fromList . fmap importMod'

importMods' :: QualId -> Set ImportDecl
importMods' = Set.singleton . importMod'

unsafeImportMods' :: [Text] -> Set ImportDecl
unsafeImportMods' = Set.fromList . fmap (importMod' . unsafeQ)

transParams :: [Id]
transParams = ["f", "g", "m", "ra", "rb", "ha", "hb"]

transType :: Type -> Type
transType t = tApps' t transParams

codecType :: Text -> Type -> Type
codecType v t = tApps' t $ fmap (<> v) ["c", "r", "h", "f"]

mkForeignCodecName :: GuguguK r m => Module -> Data -> m (Id, Id)
mkForeignCodecName Module{..} Data{..} = do
  let qName = if moduleName == "Foreign"
        then dataName else moduleName <> dataName
      f p   = mkTypeFunc $ p <> qName
  liftA2 (,) (f "encode") (f "decode")

withTransformer :: GuguguK r m
                => (GuguguNameTransformers -> NameTransformer)
                -> ((Text -> Text) -> a)
                -> m a
withTransformer selector k = do
  nt <- asks $ selector . nameTransformers . toGuguguHaskellOption
  pure . k $ runNameTransformer nt


-- Instances

instance HasGuguguHaskellOption GuguguHaskellEnv where
  toGuguguHaskellOption = gOpts

instance HasResolutionContext GuguguHaskellEnv where
  toResolutionContext = gCtx
