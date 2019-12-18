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

import           Control.Monad.Except
import           Control.Monad.Reader
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

class HasGuguguRustOption a where
  toGuguguRustOption :: a -> GuguguRustOption

makeModules :: MonadError String m
            => GuguguRustOption
            -> [Module]
            -> m (Map FilePath Crate)
makeModules opts@GuguguRustOption{..} modules = do
  let moduleMap = Map.fromList $
        fmap (\md@Module{..} -> (moduleName, md)) modules
  files <- for modules $ \md -> do
    let rCtx = ResolutionContext
          { rcModules       = moduleMap
          , rcCurrentModule = md
          }
        env  = GuguguRustEnv
          { gOpts = opts
          , gCtx  = rCtx
          }
    runReaderT (makeModule md) env
  let allFiles      = Map.fromList $
           withGroup files
        <> [(runtimePath "mod", runtimeCrate)]
      runtimeCrate  = Crate
        { cItems = []
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

makeModule :: GuguguK r m => Module -> m (FilePath, Crate)
makeModule md@Module{..} = do
  items <- traverse makeData moduleDatas
  mod' <- mkModuleCode md
  let crate = Crate{ cItems = items }
      path  = modulePath mod'
  pure (path, crate)

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

mkFieldCode :: GuguguK r m => RecordField -> m Text
mkFieldCode RecordField{..} = withTransformer transFieldCode $ \f ->
  f recordFieldName

mkEnumCode :: GuguguK r m => Text -> m Text
mkEnumCode name = withTransformer transEnumCode $ \f ->
  f name


-- Utilities

thisTarget :: Text
thisTarget = "rust"

modItem :: Identifier -> Item
modItem n = noAttr $ IModule RsModule{ mName = n }

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
