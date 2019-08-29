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
  pairs <- traverse (makeData md) moduleDatas
  pure $ Map.fromList pairs

makeData :: GuguguK r m => Module -> Data -> m (FilePath, CompilationUnit)
makeData md@Module{..} d@Data{..} = do
  GuguguScalaOption{..} <- asks toGuguguScalaOption
  dataCode <- mkTypeCode d
  typeDef <- case dataConDef of
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
      pure (TSC classDef)

  moduleCode <- mkModuleCode md

  let compilationUnit = CompilationUnit
        { cuPackage  = QualId moduleCode
        , cuTopStats = [typeDef]
        }
      path            = pkgDir moduleCode </> (T.unpack dataCode <> ".scala")
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

mkTypeCode :: GuguguK r m => Data -> m Text
mkTypeCode Data{..} = withTransformer transTypeCode $ \f ->
  f dataName

mkFieldCode :: GuguguK r m => RecordField -> m Text
mkFieldCode RecordField{..} = withTransformer transFieldCode $ \f ->
  f recordFieldName


-- Utilities

iSimple :: Text -> StableId
iSimple t = StableId $ t :| []

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
