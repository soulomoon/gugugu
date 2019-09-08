{-|
Scala target
 -}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module Gugugu.Lang.Scala
  ( GuguguScalaOption(..)
  , makeFiles
  ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Bifoldable
import           Data.Foldable
import           Data.List.NonEmpty            (NonEmpty (..))
import qualified Data.List.NonEmpty            as NonEmpty
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Traversable
import           System.FilePath

import           Gugugu.Resolver
import           Gugugu.Utilities

import           Gugugu.Lang.Scala.SourceUtils


-- | Option for 'makeFiles'
data GuguguScalaOption
  = GuguguScalaOption
    { packagePrefix    :: [Text]                  -- ^ Package prefix
    , runtimePkg       :: [Text]                  -- ^ Gugugu runtime package
    , withCodec        :: Bool                    -- ^ True if generate codec
    , withServer       :: Bool                    -- ^ True if generate server
    , withClient       :: Bool                    -- ^ True if generate client
    , nameTransformers :: GuguguNameTransformers  -- ^ Name transformers
    }
  deriving Show

-- | Make Scala AST from 'Module's
makeFiles :: MonadError String m
          => GuguguScalaOption
          -> [Module]
          -> m (Map FilePath CompilationUnit)
makeFiles = makeModules


type GuguguK r m = ( MonadError String m
                   , MonadReader r m
                   , HasGuguguScalaOption r
                   , HasResolutionContext r
                   )

data GuguguScalaEnv
  = GuguguScalaEnv
    { gsOpts :: GuguguScalaOption
    , gsCtx  :: ResolutionContext
    }
  deriving Show

class HasGuguguScalaOption a where
  toGuguguScalaOption :: a -> GuguguScalaOption


makeModules :: MonadError String m
            => GuguguScalaOption
            -> [Module]
            -> m (Map FilePath CompilationUnit)
makeModules opts modules = do
  let moduleMap = Map.fromList $
        fmap (\md@Module{..} -> (moduleName, md)) modules
  filesMaps <- for modules $ \md@Module{..} -> do
    let rCtx = ResolutionContext
          { rcModules       = moduleMap
          , rcCurrentModule = md
          }
        env  = GuguguScalaEnv
          { gsOpts = opts
          , gsCtx  = rCtx
          }
    runReaderT (makeModule md) env
  pure $ Map.unions filesMaps

makeModule :: GuguguK r m => Module -> m (Map FilePath CompilationUnit)
makeModule md@Module{..} = do
  GuguguScalaOption{..} <- asks toGuguguScalaOption
  pairs <- traverse (makeData md) moduleDatas
  tPair <- if (withServer || withClient) && not (null moduleFuncs)
    then Just <$> makeTransport md else pure Nothing
  pure $ Map.fromList $ toList tPair <> pairs

makeData :: GuguguK r m => Module -> Data -> m (FilePath, CompilationUnit)
makeData md@Module{..} d@Data{..} = do
  GuguguScalaOption{..} <- asks toGuguguScalaOption
  dataCode <- mkTypeCode d
  (typeDef, maybeObjWithoutCodec) <- case dataConDef of
    DRecord RecordCon{..} -> do
      params <- for recordConFields $ \rf@RecordField{..} -> do
        scalaType <- makeType recordFieldType
        fieldCode <- mkFieldCode rf
        pure Param
          { pName = fieldCode
          , pType = scalaType
          }
      let classDef = ClassDef
            { cdModifiers = [MCase]
            , cdName      = dataCode
            , cdParams    = params
            }
      pure (TSC classDef, Nothing)

  codecStats <- if withCodec then makeCodecStats d else pure []

  moduleCode <- mkModuleCode md

  let compilationUnit = CompilationUnit
        { cuPackage    = QualId moduleCode
        , cuPkgImports = []
        , cuTopStats   = [typeDef] <> toList (fmap TSO mObjectDef)
        }
      path            = pkgDir moduleCode </> (T.unpack dataCode <> ".scala")
      mObjectDef      = if null codecStats
        then maybeObjWithoutCodec
        else Just $ case maybeObjWithoutCodec of
          Just od -> od{ odBody = odBody od <> codecStats }
          Nothing -> ObjectDef
            { odModifiers = []
            , odName      = dataCode
            , odBody      = codecStats
            }
  pure (path, compilationUnit)

makeCodecStats :: GuguguK r m => Data -> m [TemplateStat]
makeCodecStats d@Data{..} = do
  codecPkg <- asks guguguCodecPkg
  dataCode <- mkTypeCode d
  let eImpl         = eSimple "impl"
      encoderTypeId = codecPkgId "Encoder"
      decoderTypeId = codecPkgId "Decoder"
      eS            = eSimple "s"
      eA            = eSimple "a"
      -- scala type of this type
      tThis         = tSimple dataCode
      codecPkgId n  = StableId $ codecPkg <> (n :| [])
  (encodeFDef, decodeFDef) <- case dataConDef of
    DRecord RecordCon{..} -> do
      let eEncodeRecordField = eImpl `EMember` "encodeRecordField"
          eDecodeRecordField = eImpl `EMember` "decodeRecordField"
          eEncoderObj        = ESimple encoderTypeId
          eDecoderObj        = ESimple decoderTypeId
          e_                 = eSimple "_"
      codecComps <- for (indexed recordConFields) $ \(i, rf) -> do
        t <- makeType $ recordFieldType rf
        fieldCode <- mkFieldCode rf
        fieldValue <- mkFieldValue rf
        let encodeDef  = PatDef
              { pdModifiers = []
              , pdPattern   = PSimple sn
              , pdType      = Nothing
              , pdDef       = ECall eEncodeRecordField
                  [ eSPrevious, eI, fieldValue
                  , ECall (eTCall1 eEncoderObj t `EMember` "encode")
                      [e_, eA `EMember` fieldCode, eImpl]
                  ]
              }
            decodeDef  = PatDef
              { pdModifiers = []
              , pdPattern   = PTuple $ sn :| [vn]
              , pdType      = Nothing
              , pdDef       = ECall eDecodeRecordField
                  [ eSPrevious, eI, fieldValue
                  , ECall (eTCall1 eDecoderObj t `EMember` "decode")
                      [e_, eImpl]
                  ]
              }
            eN         = eSimple vn
            eI         = eSimple $ showText i
            eSPrevious = eSimple $ "s" <> showText (i + 1)
            sn         = "s" <> showText (i + 2)
            vn         = "v" <> showText i
        pure (encodeDef, decodeDef, eN)
      let (encodeDefs, decodeDefs, fieldNames) = unzip3 codecComps
      let encodeFDef = ECall (eImpl `EMember` "encodeRecord")
            [eS, eN, eAnon1 "s1" $ EBlock $ fmap BSP encodeDefs <> [BSE eSl]]
          decodeFDef = ECall (eImpl `EMember` "decodeRecord")
            [eS, eN, eAnon1 "s1" $ EBlock $ fmap BSP decodeDefs <> [BSE eR]]
            where eR = eTuple2 eSl $ ECall (eSimple dataCode) fieldNames
          eN         = eSimple $ showText nFields
          -- last s
          eSl        = eSimple $ "s" <> showText (nFields + 1)
          nFields    = length recordConFields
      pure (encodeFDef, decodeFDef)
  let encoderDef = PatDef
        { pdModifiers = [MImplicit]
        , pdPattern   = PSimple $ "encode" <> dataCode
        , pdType      = Just t
        , pdDef       = ENew t $ Just [TMSF encodeDef]
        }
        where t = TParamed encoderTypeId [tThis]
      decoderDef = PatDef
        { pdModifiers = [MImplicit]
        , pdPattern   = PSimple $ "decode" <> dataCode
        , pdType      = Just t
        , pdDef       = ENew t $ Just [TMSF decodeDef]
        }
        where t = TParamed decoderTypeId [tThis]
      encodeDef  = FunDef
        { fdSig = FunDcl
            { fdModifiers = [MOverride]
            , fdName      = "encode"
            , fdTParams   = ["S", "R"]
            , fdParams    = [pS, fp, pImpl "EncoderImpl"]
            , fdRType     = tS
            }
        , fdDef = encodeFDef
        }
        where fp = Param
                { pName = "a"
                , pType = tThis
                }
      decodeDef  = FunDef
        { fdSig = FunDcl
            { fdModifiers = [MOverride]
            , fdName      = "decode"
            , fdTParams   = ["S", "R"]
            , fdParams    = [pS, pImpl "DecoderImpl"]
            , fdRType     = TTuple $ tS :| [tThis]
            }
        , fdDef = decodeFDef
        }

      pS         = Param
        { pName = "s"
        , pType = tS
        }
      pImpl bn   = Param
        { pName = "impl"
        , pType = TParamed (codecPkgId bn) [tS, tSimple "R"]
        }
      tS         = tSimple "S"

  pure [TMSV encoderDef, TMSV decoderDef]

makeTransport :: GuguguK r m => Module -> m (FilePath, CompilationUnit)
makeTransport md@Module{..} = do
  GuguguScalaOption{..} <- asks toGuguguScalaOption
  transportPkg <- asks guguguTransportPkg
  codecPkg <- asks guguguCodecPkg
  let eEncode             = ESimple (codecPkgId "Encoder") `EMember` "encode"
      eDecode             = ESimple (codecPkgId "Decoder") `EMember` "decode"
      qualNameId          = transportPkgId "QualName"
      codecPkgId n        = StableId $ codecPkg <> (n :| [])
      transportPkgId n    = StableId $ transportPkg <> (n :| [])
      e_                  = eSimple "_"
      eK                  = eSimple "k"
      eImpl               = eSimple "impl"
      eNamespace          = eSimple "namespace"
      eTransportSend      = eSimple "transport" `EMember` "send"
      eFEncode (tS, tR) t = ECall (ETCall eEncode (tS :| [tR, t]))
        [e_, eEncoderImpl]
      eFDecode (tS, tR) t = ECall (ETCall eDecode (tS :| [tR, t]))
        [e_, eDecoderImpl]
      eEncoderImpl        = eSimple "encoderImpl"
      eDecoderImpl        = eSimple "decoderImpl"
      eFa                 = eSimple "fa"
      tSRA                = (tSimple "SA", tSimple "RA")
      tSRB                = (tSimple "SB", tSimple "RB")

  transportComps <- for moduleFuncs $ \fn@Func{..} -> do
    fd <- makeType funcDomain
    fcd <- case funcCodomain of
      GApp{ typeCon = "IO", typeParams = [v] } -> makeType v
      _                                        ->
        throwError "Function codomain must be a type like IO a"
    funcCode <- mkFuncCode fn
    funcValue <- mkFuncValue fn
    let traitFunc  = FunDcl
          { fdModifiers = []
          , fdName      = funcCode
          , fdTParams   = []
          , fdParams    = [Param{ pName = "fa", pType = inputType }]
          , fdRType     = outputType
          }
        toPattern  = PSimple funcValue
        toExpr     = ECall (eSimple "Some")
          [ ECall (eTCall2 eK fd fcd)
              [ e_
              , eFDecode tSRA fd
              , eFEncode tSRB fcd
              , eImpl `EMember` funcCode
              ]
          ]
        fromDef    = FunDef
          { fdSig = traitFunc
          , fdDef = ECall (eTCall2 eTransportSend fd fcd)
              [ ECall (ESimple qualNameId) [eNamespace, eSimple funcValue]
              , eFa
              , eFEncode tSRA fd
              , eFDecode tSRB fcd
              ]
          }
        inputType  = tSimple1 "F" fd
        outputType = tSimple1 "M" $ tSimple1 "G" fcd
    pure (traitFunc, (toPattern, toExpr), fromDef)
  let (traitFuncs, toCases, fromDefs) = unzip3 transportComps
  moduleCode <- mkModuleCode md
  moduleValue <- mkModuleValue md
  traitName <- mkModuleType md
  let compilationUnit = CompilationUnit
        { cuPackage    = QualId moduleCode
        , cuPkgImports = [(QualId ("scala" :| ["language"]), ["higherKinds"])]
        , cuTopStats   = [TST traitDef, TSO objectDef]
        }
      path            = pkgDir moduleCode </> (T.unpack traitName <> ".scala")
      traitDef        = TraitDef
        { tdName    = traitName
        , tdTParams = ["F[_]", "G[_]", "M[_]"]
        , tdBody    = fmap TMSD traitFuncs
        }
      objectDef       = ObjectDef
        { odModifiers = []
        , odName      = traitName
        , odBody      = [TMSV nsDef]
            <> [TMSF toTransport | withServer]
            <> [TMSF fromTransport | withClient]
        }
      nsDef           = PatDef
        { pdModifiers = []
        , pdPattern   = PSimple "namespace"
        , pdType      = Just $ tSimple1 "Vector" $ tSimple "String"
        , pdDef       = moduleValue
        }
      toTransport     = FunDef
        { fdSig = FunDcl
            { fdModifiers = []
            , fdName      = "toTransport"
            , fdTParams   = ["F[_]", "G[_]", "M[_]", "RA", "RB", "SA", "SB"]
            , fdParams    =
                [ Param
                    { pName = "impl"
                    , pType = tThis
                    }
                , Param
                    { pName = "decoderImpl"
                    , pType = TParamed (codecPkgId "DecoderImpl") $ biList tSRA
                    }
                , Param
                    { pName = "encoderImpl"
                    , pType = TParamed (codecPkgId "EncoderImpl") $ biList tSRB
                    }
                ]
            , fdRType     = tServerTransport
            }
        , fdDef = ENew tServerTransport $ Just [TMSF transportAsk]
        }
        where tServerTransport =
                TParamed (transportPkgId "ServerTransport") tParams
      fromTransport   = FunDef
        { fdSig = FunDcl
            { fdModifiers = []
            , fdName      = "fromTransport"
            , fdTParams   = ["F[_]", "G[_]", "M[_]", "RA", "RB", "SA", "SB"]
            , fdParams    =
                [ Param
                    { pName = "transport"
                    , pType =
                        TParamed (transportPkgId "ClientTransport") tParams
                    }
                , Param
                    { pName = "encoderImpl"
                    , pType = TParamed (codecPkgId "EncoderImpl") $ biList tSRA
                    }
                , Param
                    { pName = "decoderImpl"
                    , pType = TParamed (codecPkgId "DecoderImpl") $ biList tSRB
                    }
                ]
            , fdRType     = tThis
            }
        , fdDef = ENew tThis $ Just $ fmap TMSF fromDefs
        }
      transportAsk    = FunDef
        { fdSig = FunDcl
            { fdModifiers = [MOverride]
            , fdName      = "ask"
            , fdTParams   = []
            , fdParams    =
                [ Param
                    { pName = "name"
                    , pType = TParamed qualNameId [tSimple "String"]
                    }
                , Param
                    { pName = "k"
                    , pType =
                        TParamed (transportPkgId "ServerCodecHandler") tParams
                    }
                ]
            , fdRType     = tSimple1 "Option" $ TFun
                [tSimple1 "F" $ tSimple "RA"]
                (tSimple1 "M" $ tSimple1 "G" $ tSimple "RB")
            }
        , fdDef = EBlock
            [ BSE $ EIf cond (EReturn (eSimple "None")) Nothing
            , BSE $ EMatch (eSimple "name" `EMember` "name") $
                toCases <> [(PSimple "_", eSimple "None")]
            ]
        }
        where cond = EBinary (eSimple "name" `EMember` "namespace")
                        "!=" eNamespace
      tThis           = TParamed (iSimple traitName)
        [tSimple "F", tSimple "G", tSimple "M"]
      tParams         = fmap tSimple ["F", "G", "M", "RA", "RB"]
  pure (path, compilationUnit)


makeType :: GuguguK r m => GType -> m Type
makeType GApp{..} = do
  tFirst <- resolveScalaType typeCon
  params <- traverse makeType typeParams
  pure $ TParamed tFirst params

resolveScalaType :: GuguguK r m => Text -> m StableId
resolveScalaType t = do
  rr <- resolveTypeCon t
  case rr of
    ResolutionError e -> throwError e
    LocalType d       -> iSimple <$> mkTypeCode d
    Primitive pt      -> pure $ iSimple $ case pt of
      PUnit   -> "Unit"
      PBool   -> "Boolean"
      PInt32  -> "Int"
      PDouble -> "Double"
      PString -> "String"
      PMaybe  -> "Option"
      PList   -> "Vector"

pkgDir :: NonEmpty Text -> FilePath
pkgDir (part1 :| parts) = foldl' (\z t -> z </> T.unpack t)
                                 (T.unpack part1) parts


-- Name transformers

mkModuleCode :: GuguguK r m => Module -> m (NonEmpty Text)
mkModuleCode Module{..} = do
  GuguguScalaOption{..} <- asks toGuguguScalaOption
  withTransformer transModuleCode $ \f ->
    NonEmpty.fromList $ packagePrefix <> [f moduleName]

mkModuleValue :: GuguguK r m => Module -> m Expr
mkModuleValue Module{..} = withTransformer transModuleValue $ \f ->
  ECall (eSimple "Vector") [eSimple $ unsafeQuote $ f moduleName]

mkModuleType :: GuguguK r m => Module -> m Text
mkModuleType Module{..} = withTransformer transModuleType $ \f ->
  f $ moduleName <> "Module"

mkFuncCode :: GuguguK r m => Func -> m Text
mkFuncCode Func{..} = withTransformer transFuncCode $ \f ->
  f funcName

mkFuncValue :: GuguguK r m => Func -> m Text
mkFuncValue Func{..} = withTransformer transFuncValue $ \f ->
  unsafeQuote $ f funcName

mkTypeCode :: GuguguK r m => Data -> m Text
mkTypeCode Data{..} = withTransformer transTypeCode $ \f ->
  f dataName

mkFieldCode :: GuguguK r m => RecordField -> m Text
mkFieldCode RecordField{..} = withTransformer transFieldCode $ \f ->
  f recordFieldName

mkFieldValue :: GuguguK r m => RecordField -> m Expr
mkFieldValue RecordField{..} = withTransformer transFieldValue $ \f ->
  eSimple $ unsafeQuote $ f recordFieldName


-- Utilities

guguguCodecPkg :: HasGuguguScalaOption r => r -> NonEmpty Text
guguguCodecPkg r =
  let GuguguScalaOption{..} = toGuguguScalaOption r
  in NonEmpty.fromList $ runtimePkg <> ["codec"]

guguguTransportPkg :: HasGuguguScalaOption r => r -> NonEmpty Text
guguguTransportPkg r =
  let GuguguScalaOption{..} = toGuguguScalaOption r
  in NonEmpty.fromList $ runtimePkg <> ["transport"]

iSimple :: Text -> StableId
iSimple t = StableId $ t :| []

eSimple :: Text -> Expr
eSimple = ESimple . iSimple

eAnon1 :: Text -> Expr -> Expr
eAnon1 t = EAnon $ t :| []

eTuple2 :: Expr -> Expr -> Expr
eTuple2 t1 t2 = ETuple $ t1 :| [t2]

eTCall1 :: Expr -> Type -> Expr
eTCall1 expr t = ETCall expr $ t :| []

eTCall2 :: Expr -> Type -> Type -> Expr
eTCall2 expr t1 t2 = ETCall expr $ t1 :| [t2]

tSimple :: Text -> Type
tSimple t = TParamed (iSimple t) []

tSimple1 :: Text -> Type -> Type
tSimple1 t tp = TParamed (iSimple t) [tp]

withTransformer :: GuguguK r m
                => (GuguguNameTransformers -> NameTransformer)
                -> ((Text -> Text) -> a)
                -> m a
withTransformer selector k = do
  nt <- asks $ selector . nameTransformers . toGuguguScalaOption
  pure . k $ runNameTransformer nt


-- Instances

instance HasGuguguScalaOption GuguguScalaEnv where
  toGuguguScalaOption = gsOpts

instance HasResolutionContext GuguguScalaEnv where
  toResolutionContext = gsCtx
