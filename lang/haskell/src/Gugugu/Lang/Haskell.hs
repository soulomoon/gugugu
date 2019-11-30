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
  let allFiles    = Map.fromList $
           files
        <> [(codecPath, codecModule) | withCodec]
      codecModule = HaskellModule
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
      codecPath   = modulePath codecModId
      codecModId  = NonEmpty.fromList $ runtimeMod <> ["Codec"]
  pure allFiles

makeModule :: GuguguK r m
           => Module
           -> m (ForeignItems, (FilePath, HaskellModule))
makeModule md@Module{..} = do
  GuguguHaskellOption{..} <- asks toGuguguHaskellOption
  importsAndDecs <- traverse makeData moduleDatas
  importsAndInstDecs <- if withCodec
    then traverse (makeCodecInsts md) moduleDatas else pure mempty
  mod' <- mkModuleCode md
  let (imports, decs) = fold importsAndDecs
  let (foreignItems, (imports', instDecs)) = fold importsAndInstDecs
  let moduleBody = HaskellModule
        { hmExts    = ["OverloadedStrings" | withCodec]
        , hmId      = mod'
        , hmImports = Set.toAscList $ imports <> imports'
        , hmDecls   = decs <> instDecs
        }
      path       = modulePath mod'
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

importMod' :: QualId -> ImportDecl
importMod' qid = ImportDecl{ idModuleId = qid }

importMods' :: QualId -> Set ImportDecl
importMods' = Set.singleton . importMod'

unsafeImportMods' :: [Text] -> Set ImportDecl
unsafeImportMods' = Set.fromList . fmap (importMod' . unsafeQ)

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
