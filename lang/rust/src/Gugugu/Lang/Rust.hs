{-|
Rust target
 -}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module Gugugu.Lang.Rust
  ( GuguguRustOption(..)
  , makeFiles
  ) where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Bifunctor
import           Data.Foldable
import qualified Data.List                    as List
import           Data.List.NonEmpty           (NonEmpty (..))
import qualified Data.List.NonEmpty           as NonEmpty
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Traversable
import           System.FilePath
import           Text.Printf

import           Gugugu.Resolver
import           Gugugu.Utilities

import           Gugugu.Lang.Rust.SourceUtils

-- | Option for 'makeFiles'
data GuguguRustOption
  = GuguguRustOption
    { modulePrefix     :: [Text]                  -- ^ Module prefix
    , runtimeMod       :: [Text]                  -- ^ Runtime module
    , derivings        :: [Text]                  -- ^ derive attribute
    , withCodec        :: Bool                    -- ^ True if generate codec
    , nameTransformers :: GuguguNameTransformers  -- ^ Name transformers
    }
  deriving Show

-- | Make Rust AST from 'Module's
makeFiles :: MonadError String m
          => GuguguRustOption
          -> [Module]
          -> m (Map FilePath Crate)
makeFiles = makeModules


type GuguguK r m = ( MonadError String m
                   , MonadReader r m
                   , HasGuguguRustOption r
                   , HasResolutionContext r
                   )

data GuguguRustEnv
  = GuguguRustEnv
    { gOpts :: GuguguRustOption
    , gCtx  :: ResolutionContext
    }
  deriving Show

type ForeignItems = ([TraitItem], [TraitItem])

class HasGuguguRustOption a where
  toGuguguRustOption :: a -> GuguguRustOption

makeModules :: MonadError String m
            => GuguguRustOption
            -> [Module]
            -> m (Map FilePath Crate)
makeModules opts@GuguguRustOption{..} modules = do
  let moduleMap = Map.fromList $
        fmap (\md@Module{..} -> (moduleName, md)) modules
  foreignAndFiles <- for modules $ \md -> do
    let rCtx = ResolutionContext
          { rcModules       = moduleMap
          , rcCurrentModule = md
          }
        env  = GuguguRustEnv
          { gOpts = opts
          , gCtx  = rCtx
          }
    runReaderT (makeModule md) env
  let (foreigns, files) = unzip foreignAndFiles
  let (foreignEncodes, foreignDecodes) = fold foreigns
  let allFiles      = Map.fromList $
           withGroup files
        <> [(runtimePath "mod", runtimeCrate)]
        <> [(runtimePath "foreign", foreignCrate) | withCodec]
      runtimeCrate  = Crate
        { cItems = fmap modItem $
               (if withCodec then ["codec", "foreign"] else [])
        }
      foreignCrate  = Crate
        { cItems =
            [ fTrait "ForeignEncodersImpl" foreignEncodes
            , fTrait "ForeignDecodersImpl" foreignDecodes
            ]
        }
        where
          fTrait n fs = noAttr $ ITrait Trait
            { tName    = n
            , tTParams = []
            , tItems   = fmap ttSimple ["Error", "State"] <> fs
            }
          ttSimple n  = TT TraitType
            { ttName   = n
            , ttBounds = []
            , ttBody   = Nothing
            }
      runtimePath n = modulePath $ NonEmpty.fromList $ runtimeMod <> [n]
      withGroup fs  = case NonEmpty.nonEmpty modulePrefix of
        Just groupMod ->
          let path        = modulePath $ groupMod <> ("mod" :| [])
              crate       = Crate{ cItems = fmap modItem names }
              names       = List.sort $ fmap (transformer . moduleName) modules
              transformer =
                runNameTransformer (transModuleCode nameTransformers)
          in (path, crate) : fs
        Nothing       -> fs
  pure allFiles

makeModule :: GuguguK r m => Module -> m (ForeignItems, (FilePath, Crate))
makeModule md@Module{..} = do
  GuguguRustOption{..} <- asks toGuguguRustOption
  items <- traverse makeData moduleDatas
  foreignAndCodecItems <- if withCodec
    then traverse (makeCodecImpls md) moduleDatas else pure mempty
  mod' <- mkModuleCode md
  let (foreigns, codecItems) = fold foreignAndCodecItems
  let crate = Crate{ cItems = items <> codecItems }
      path  = modulePath mod'
  pure (foreigns, (path, crate))

makeData :: GuguguK r m => Data -> m Item
makeData d@Data{..} = do
  GuguguRustOption{..} <- asks toGuguguRustOption
  dataCode <- mkTypeCode d
  case dataConDef of
    Just dataCon -> do
      visItem <- case dataCon of
        DRecord RecordCon{..} -> do
          fields <- for recordConFields $ \rf@RecordField{..} -> do
            fieldCode <- mkFieldCode rf
            rsType <- makeType recordFieldType
            let sf = StructField
                  { sfName = fieldCode
                  , sfType = rsType
                  }
            pure sf
          let struct = IStruct Struct
                { sName   = dataCode
                , sFields = fields
                }
          pure struct
        DEnum names           -> do
          enumItems <- for (toList names) $ \name -> do
            enumCode <- mkEnumCode name
            pure enumCode
          let struct = IEnumeration Enumeration
                { eName  = dataCode
                , eItems = enumItems
                }
          pure struct
      pure $ Item [OuterAttribute $ ADerive $ fmap TSimple derivings] visItem
    Nothing      -> do
      tPath <- resolveForeign' d
      let struct = noAttr $ ITypeAlias TypeAlias
            { taName = dataCode
            , taType = either TSimple TPath tPath
            }
      pure struct

makeCodecImpls :: GuguguK r m => Module -> Data -> m (ForeignItems, [Item])
makeCodecImpls md d@Data{..} = do
  GuguguRustOption{..} <- asks toGuguguRustOption
  dataCode <- mkTypeCode d
  let thisType = TSimple dataCode
      eCallC   = EMethod $ ESimple "c"
      eS       = ESimple "s"
      codecT n = TPath $ NonEmpty.fromList $
        ["crate"] <> runtimeMod <> ["codec", n]
  (foreignItems, encodeExpr, decodeExpr) <- case dataConDef of
    Just (DRecord RecordCon{..}) -> do
      let eCallC1 = EMethod $ ESimple "c1"
      codecComps <- for (indexed recordConFields) $ \(i, rf) -> do
        fieldCode <- mkFieldCode rf
        fieldValue <- mkFieldValue rf
        let encodeStmt  = SLet sn $ EPropagate $
              eCallC1 "encode_record_field"
                [ eSPrevious
                , eI
                , fieldValue
                , EBorrow $ ESimple "a1" `EField` fieldCode
                ]
            decodeStmt  = SLet (PTuple [sn, PSimple vn]) $ EPropagate $
              eCallC1 "decode_record_field"
                [ eSPrevious
                , eI
                , fieldValue
                ]
            decodeField = (fieldCode, ESimple vn)
            eI          = ESimple $ showText i
            eSPrevious  = ESimple $ "s" <> showText (i + 1)
            sn          = PSimple $ "s" <> showText (i + 2)
            vn          = "v" <> showText i
        pure (encodeStmt, decodeStmt, decodeField)
      let (encodeStmts, decodeStmts, decodeFields) = unzip3 codecComps
      let encodeExpr = eCallC "encode_record"
            [ eS
            , eN
            , ESimple "a"
            , EClosure ["c1", "s1", "a1"] $ EBlock encodeStmts $ ok eSl
            ]
          decodeExpr = eCallC "decode_record"
            [ eS
            , eN
            , EClosure ["c1", "s1"] $ EBlock decodeStmts $ ok $
                ETuple [eSl, EStruct (ESimple dataCode) decodeFields]
            ]
          ok e       = ESimple "Ok" `ECall` [e]
          eN         = ESimple $ showText nFields
          eSl        = ESimple $ "s" <> showText (nFields + 1)
          nFields    = length recordConFields
      pure (mempty, encodeExpr, decodeExpr)
    Just (DEnum names)           -> do
      codecCases <- for (indexed $ toList names) $ \(i, name) -> do
        enumCode <- mkEnumCode name
        enumValue <- mkEnumValue name
        let encodeCase = (PPath enumPath, (ESimple cI, ESimple enumValue))
            decodeCase = ((PSimple cI, PSimple enumValue), decoded)
            enumPath   = dataCode :| [enumCode]
            decoded    = ESimple "Some" `ECall` [EPath enumPath]
            cI         = showText i
        pure (encodeCase, decodeCase)
      let (encodeCases, decodeCases) = unzip codecCases
      let encodeExpr   = eCallC "encode_enum"
            [ eS
            , ESimple "a"
            , asF fst
            , asF snd
            ]
            where asF selector = EClosure ["x"] $ EMatch (ESimple "x") $
                       fmap (second selector) encodeCases
          decodeExpr   = eCallC "decode_enum"
            [ eS
            , byF "i" fst
            , byF "n" snd
            ]
            where byF arg selector = EClosure [arg] $ EMatch (ESimple arg) $
                       fmap (first selector) decodeCases
                    <> [(PSimple "_", ESimple "None")]
      pure (mempty, encodeExpr, decodeExpr)
    Nothing                      -> do
      tPath <- resolveForeign' d
      (encodeName, decodeName) <- mkForeignCodecName md d
      let encodeExpr    = eCallC encodeName [eS, ESimple "a"]
          decodeExpr    = eCallC decodeName [eS]
          foreignItems  = ([foreignEncode], [foreignDecode])
          foreignEncode = TF Function
            { fName    = encodeName
            , fTParams = []
            , fParams  =
                [fpRefSelf,  fpSimple "s" tS, fpSimple "v" $ TRef rsType]
            , fRType   = TSimple "Result" `tParam` [tS, tE]
            , fWhere   = []
            , fBody    = Nothing
            }
          foreignDecode  = TF Function
            { fName    = decodeName
            , fTParams = []
            , fParams  = [fpRefSelf,  fpSimple "s" tS]
            , fRType   = TSimple "Result" `tParam` [TTuple [tS, rsType], tE]
            , fWhere   = []
            , fBody    = Nothing
            }
          tS     = TPath $ "Self" :| ["State"]
          tE     = TPath $ "Self" :| ["Error"]
          rsType = either TSimple TPath tPath
      pure (foreignItems, encodeExpr, decodeExpr)
  let allItems   = fmap (noAttr . ITraitImpl) [encodeImpl, decodeImpl]
      encodeImpl = codecImpl "Encoding" "EncoderImpl" "encode"
        [fpSimple "a" $ TRef thisType] tS encodeExpr
      decodeImpl = codecImpl "Decoding" "DecoderImpl" "decode"
        [] (TTuple [tS, thisType]) decodeExpr
      codecImpl traitName implName funcName param rtype expr = TraitImpl
        { tiTrait   = codecT traitName
        , tiTParams = []
        , tiFor     = thisType
        , tiWhere   = []
        , tiItems   = [func]
        }
        where func = TF Function
                { fName    = funcName
                , fTParams = ["C"]
                , fParams  = [fpSimple "s" tS]
                          <> param
                          <> [fpSimple "c" $ TRef $ TSimple "C"]
                , fRType   = tResult $ rtype
                , fWhere   = [(tC, [codecT implName])]
                , fBody    = Just expr
                }
      tResult t  = tParam (TSimple "Result") [t, tE]
      tS         = TPath $ "C" :| ["State"]
      tE         = TPath $ "C" :| ["Error"]
      tC         = TSimple "C"
  pure (foreignItems, allItems)


makeType :: GuguguK r m => GType -> m Type
makeType GApp{..} = do
  r <- resolveRustType typeCon
  params <- traverse makeType typeParams
  let tCon   = either TSimple TPath r
      rsType = if null params then tCon else tParam tCon params
  pure rsType

resolveRustType :: GuguguK r m => Text -> m (Either Text (NonEmpty Identifier))
resolveRustType t = do
  rr <- resolveTypeCon t
  case rr of
    ResolutionError e -> throwError e
    LocalType d       -> do
      typeName <- mkTypeCode d
      pure $ Left typeName
    Imported md d     -> do
      modName <- mkModuleCode md
      typeName <- mkTypeCode d
      pure $ Right $ ("crate" :| []) <> modName <> (typeName :| [])
    Primitive pt      ->
      pure . Left $ case pt of
        PUnit   -> "()"
        PBool   -> "bool"
        PInt32  -> "i32"
        PDouble -> "f64"
        PString -> "String"
        PMaybe  -> "Option"
        PList   -> "Vec"

resolveForeign' :: GuguguK r m => Data -> m (Either Text (NonEmpty Identifier))
resolveForeign' Data{..} = do
  foreignPragma <- case Map.lookup thisTarget dataForeignMap of
    Just v  -> pure v
    Nothing -> throwError $ printf
      "Type %s does not have foreign pragma" dataName
  case NonEmpty.nonEmpty $ splitOn' "::" foreignPragma of
    Nothing -> throwError $ printf "Bad FOREIGN pragma: %s" foreignPragma
    Just qn -> pure $ case qn of
      n :| [] -> Left n
      _       -> Right qn

modulePath :: NonEmpty Identifier -> FilePath
modulePath (part1 :| parts) =
  foldl' (\z t -> z </> T.unpack t) (T.unpack part1) parts <.> "rs"


-- Name transformers

mkModuleCode :: GuguguK r m => Module -> m (NonEmpty Identifier)
mkModuleCode Module{..} = do
  GuguguRustOption{..} <- asks toGuguguRustOption
  withTransformer transModuleCode $ \f ->
    NonEmpty.fromList $ modulePrefix <> [f moduleName]

mkTypeCode :: GuguguK r m => Data -> m Text
mkTypeCode Data{..} = withTransformer transTypeCode $ \f ->
  f dataName

mkTypeFunc :: GuguguK r m => Text -> m Text
mkTypeFunc name = withTransformer transTypeFunc $ \f ->
  f name

mkFieldCode :: GuguguK r m => RecordField -> m Text
mkFieldCode RecordField{..} = withTransformer transFieldCode $ \f ->
  f recordFieldName

mkFieldValue :: GuguguK r m => RecordField -> m Expression
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
thisTarget = "rust"

mkForeignCodecName :: GuguguK r m
                   => Module
                   -> Data
                   -> m (Identifier, Identifier)
mkForeignCodecName Module{..} Data{..} = do
  let qName = if moduleName == "Foreign"
        then dataName else moduleName <> dataName
      f p   = mkTypeFunc $ p <> qName
  liftA2 (,) (f "encode") (f "decode")

modItem :: Identifier -> Item
modItem n = noAttr $ IModule RsModule{ mName = n }

fpRefSelf :: FunctionParam
fpRefSelf = fpSimple "self" $ TRef $ TSimple "Self"

fpSimple :: Identifier -> Type -> FunctionParam
fpSimple p t = FunctionParam
  { fpPattern = PSimple p
  , fpType    = t
  }

tParam :: Type -> [Type] -> Type
tParam t tps = TParam t tps []

noAttr :: VisItem -> Item
noAttr = Item []

withTransformer :: GuguguK r m
                => (GuguguNameTransformers -> NameTransformer)
                -> ((Text -> Text) -> a)
                -> m a
withTransformer selector k = do
  nt <- asks $ selector . nameTransformers . toGuguguRustOption
  pure . k $ runNameTransformer nt


-- Instances

instance HasGuguguRustOption GuguguRustEnv where
  toGuguguRustOption = gOpts

instance HasResolutionContext GuguguRustEnv where
  toResolutionContext = gCtx
