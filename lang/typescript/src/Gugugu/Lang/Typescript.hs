{-|
Typescript target
 -}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module Gugugu.Lang.Typescript
  ( GuguguTsOption(..)
  , makeFiles
  ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Foldable
import           Data.List.NonEmpty                 (NonEmpty (..))
import qualified Data.List.NonEmpty                 as NonEmpty
import           Data.Map.Strict                    (Map)
import qualified Data.Map.Strict                    as Map
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Traversable
import           System.FilePath

import           Gugugu.Resolver
import           Gugugu.Utilities

import           Gugugu.Lang.Typescript.SourceUtils


-- | Option for 'makeFiles'
data GuguguTsOption
  = GuguguTsOption
    { packagePrefix    :: [Text]                  -- ^ Package prefix
    , nameTransformers :: GuguguNameTransformers  -- ^ Name transformers
    }
  deriving Show

-- | Make Typescript AST from 'Module's
makeFiles :: MonadError String m
          => GuguguTsOption
          -> [Module]
          -> m (Map FilePath ImplementationModule)
makeFiles = makeModules


type GuguguK r m = ( MonadError String m
                   , MonadReader r m
                   , HasGuguguTsOption r
                   , HasResolutionContext r
                   )

data GuguguTsEnv
  = GuguguTsEnv
    { gOpts :: GuguguTsOption
    , gCtx  :: ResolutionContext
    }
  deriving Show

class HasGuguguTsOption a where
  toGuguguTsOption :: a -> GuguguTsOption


makeModules :: MonadError String m
            => GuguguTsOption
            -> [Module]
            -> m (Map FilePath ImplementationModule)
makeModules opts modules = do
  let moduleMap = Map.fromList $
        fmap (\md@Module{..} -> (moduleName, md)) modules
  pairs <- for modules $ \md@Module{..} -> do
    let rCtx = ResolutionContext
          { rcModules       = moduleMap
          , rcCurrentModule = md
          }
        env  = GuguguTsEnv
          { gOpts = opts
          , gCtx  = rCtx
          }
    runReaderT (makeModule md) env
  pure $ Map.fromList pairs

makeModule :: GuguguK r m => Module -> m (FilePath, ImplementationModule)
makeModule md@Module{..} = do
  GuguguTsOption{..} <- asks toGuguguTsOption
  typeDecs <- traverse makeData moduleDatas
  tsModule <- mkTypescriptModule md
  let moduleBody = ImplementationModule
        { imBody = concat typeDecs
        }
      path       = tsModulePath tsModule <> ".ts"
  pure (path, moduleBody)

makeData :: GuguguK r m => Data -> m [ImplementationModuleElement]
makeData d@Data{..} = do
  GuguguTsOption{..} <- asks toGuguguTsOption
  dataCode <- mkTypeCode d
  dec <- case dataConDef of
    DRecord RecordCon{..} -> do
      params <- for recordConFields $ \rf@RecordField{..} -> do
        tsType <- makeType recordFieldType
        fieldCode <- mkFieldCode rf
        pure Parameter
          { pModifiers = [MPublic]
          , pName      = fieldCode
          , pType      = tsType
          }
      let classDec = ClassDeclaration
            { cdModifiers = [MExport]
            , cdName      = dataCode
            , cdBody      = [CEC classCon]
            }
          classCon = ConstructorDeclaration
            { ccdModifiers = [MPublic]
            , ccdParams    = params
            }
      pure $ MEC classDec

  let decs = [dec]
  pure decs


makeType :: GuguguK r m => GType -> m Type
makeType GApp{..} = do
  tFirst <- resolveTsType typeCon
  params <- traverse makeType typeParams
  case tFirst of
    Right t     -> pure $ TParamed t params
    Left TMaybe -> case params of
      [p] -> pure $ TUnion $ tSimple "null" :| [p]
      _   -> throwError "Maybe type requires exactly one parameter"


resolveTsType :: GuguguK r m => Text -> m (Either TSpecial NamespaceName)
resolveTsType t = do
  rr <- resolveTypeCon t
  case rr of
    ResolutionError e -> throwError e
    LocalType d       -> do
      typeName <- mkTypeCode d
      pure . Right $ nSimple typeName
    Primitive pt      -> case pt of
      PUnit   -> pure . Right $ nSimple "{}"
      PBool   -> pure . Right $ nSimple "boolean"
      PInt32  -> pure . Right $ nSimple "number"
      PDouble -> pure . Right $ nSimple "number"
      PString -> pure . Right $ nSimple "string"
      PMaybe  -> pure $ Left TMaybe
      PList   -> pure . Right $ nSimple "Array"

data TSpecial
  = TMaybe
  deriving Show

tsModulePath :: NonEmpty Text -> FilePath
tsModulePath (part1 :| parts) = foldl' (\z x -> z </> T.unpack x)
                                       (T.unpack part1)
                                       parts


-- Name transformers

mkTypescriptModule :: GuguguK r m => Module -> m (NonEmpty Text)
mkTypescriptModule Module{..} = do
  GuguguTsOption{..} <- asks toGuguguTsOption
  withTransformer transModuleCode $ \f ->
    NonEmpty.fromList $ packagePrefix <> [f moduleName]

mkTypeCode :: GuguguK r m => Data -> m Text
mkTypeCode Data{..} = withTransformer transTypeCode $ \f ->
  f dataName

mkFieldCode :: GuguguK r m => RecordField -> m Text
mkFieldCode RecordField{..} = withTransformer transFieldCode $ \f ->
  f recordFieldName


-- Utilities

nSimple :: Text -> NamespaceName
nSimple t = NamespaceName $ t :| []

tSimple :: Text -> Type
tSimple t = TParamed (nSimple t) []

withTransformer :: GuguguK r m
                => (GuguguNameTransformers -> NameTransformer)
                -> ((Text -> Text) -> a)
                -> m a
withTransformer selector k = do
  nt <- asks $ selector . nameTransformers . toGuguguTsOption
  pure . k $ runNameTransformer nt


-- Instances

instance HasGuguguTsOption GuguguTsEnv where
  toGuguguTsOption = gOpts

instance HasResolutionContext GuguguTsEnv where
  toResolutionContext = gCtx
