{-|
Python target
 -}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module Gugugu.Lang.Python
  ( GuguguPythonOption(..)
  , makeFiles
  ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Bifunctor
import           Data.Foldable
import           Data.List.NonEmpty             (NonEmpty (..))
import qualified Data.List.NonEmpty             as NonEmpty
import           Data.Map.Strict                (Map)
import qualified Data.Map.Strict                as Map
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Traversable
import           System.FilePath
import           Text.Printf

import           Gugugu.Resolver
import           Gugugu.Utilities

import           Gugugu.Lang.Python.SourceUtils


-- | Option for 'makeFiles'
data GuguguPythonOption
  = GuguguPythonOption
    { packagePrefix    :: [Text]                  -- ^ Package prefix
    , nameTransformers :: GuguguNameTransformers  -- ^ Name transformers
    }
  deriving Show

-- | Make Python AST from 'Module's
makeFiles :: MonadError String m
          => GuguguPythonOption
          -> [Module]
          -> m (Map FilePath FileInput)
makeFiles = makeModules


type GuguguK r m = ( MonadError String m
                   , MonadReader r m
                   , HasGuguguPythonOption r
                   , HasResolutionContext r
                   )

data GuguguPythonEnv
  = GuguguPythonEnv
    { gOpts :: GuguguPythonOption
    , gCtx  :: ResolutionContext
    }
  deriving Show

class HasGuguguPythonOption a where
  toGuguguPythonOption :: a -> GuguguPythonOption


makeModules :: MonadError String m
            => GuguguPythonOption
            -> [Module]
            -> m (Map FilePath FileInput)
makeModules opts@GuguguPythonOption{..} modules = do
  let moduleMap = Map.fromList $
        fmap (\md@Module{..} -> (moduleName, md)) modules
  files <- for modules $ \md@Module{..} -> do
    let rCtx = ResolutionContext
          { rcModules       = moduleMap
          , rcCurrentModule = md
          }
        env  = GuguguPythonEnv
          { gOpts = opts
          , gCtx  = rCtx
          }
    runReaderT (makeModule md) env
  let allFiles      = Map.fromList $
           [(pkgPath, pkgModule)]
        <> files
      pkgModule     = FileInput
        { fiImports = [importAnn]
        , fiContent = []
        }
      pkgPath       = modPath packagePrefix "__init__"
      modPath ns n  = modulePath $ NonEmpty.fromList $ ns <> [n]
  pure allFiles

makeModule :: GuguguK r m
           => Module
           -> m (FilePath, FileInput)
makeModule md@Module{..} = do
  importsAndTypeDecs <- traverse makeData moduleDatas
  mod' <- mkModuleCode md
  let (imports, typeDecs) = fold importsAndTypeDecs
  let moduleBody   = FileInput
        { fiImports = importAnn : Set.toAscList imports
        , fiContent = typeDecs
        }
      path         = modulePath mod'
  pure (path, moduleBody)

makeData :: GuguguK r m
         => Data
         -> m (Set ImportStmt, [Statement])
makeData d@Data{..} = do
  dataCode <- mkTypeCode d
  (imports, maybeDec) <- case dataConDef of
    Just (DRecord RecordCon{..}) -> do
      importAndfieldStmts <- for recordConFields $ \rf@RecordField{..} -> do
        fieldCode <- mkFieldCode rf
        (iField, pyType) <- makeType recordFieldType
        let stmt = SAA AnnotatedAssignmentStmt
              { aasTarget     = TSimple fieldCode
              , aasAnnotation = pyType
              }
        pure (iField, stmt)
      let classDef              = ClassDef
            { cdDecorators = [dataclass `eCall` []]
            , cdName       = dataCode
            , cdArgs       = noArg
            , cdSuite      = fieldStmts
            }
          (iDc, dataclass)      = importItem $ "dataclasses" :| ["dataclass"]
          (iFields, fieldStmts) = unzip importAndfieldStmts
      pure (fold iFields <> iDc, Just classDef)
    Just (DEnum names)           -> do
      stmts <- for (indexed $ toList names) $ \(i, name) -> do
        enumCode <- mkEnumCode name
        let stmt = SA AssignmentStmt
              { asTarget = TSimple enumCode
              , asValue  = ESimple $ showText i
              }
        pure stmt
      let classDef          = ClassDef
            { cdDecorators = []
            , cdName       = dataCode
            , cdArgs       = arg [tIntEnum]
            , cdSuite      = stmts
            }
          (iEnum, tIntEnum) = importItem $ "enum" :| ["IntEnum"]
      pure (iEnum, Just classDef)
    Nothing                      -> pure (Set.empty, Nothing)
  let decs = toList maybeDec
  pure (imports, fmap SCD decs)


makeType :: GuguguK r m => GType -> m (Set ImportStmt, Expr)
makeType GApp{..} = do
  rFirst@(imports, tFirst) <- resolvePythonType typeCon
  if null typeParams
    then pure rFirst
    else do
      importsAndParams <- traverse makeType typeParams
      let pType              = ESub tFirst params
          allImports         = fold imports' `Set.union` imports
          (imports', params) = unzip importsAndParams
      pure (allImports, pType)

resolvePythonType :: GuguguK r m => Text -> m (Set ImportStmt, Expr)
resolvePythonType t = do
  rr <- resolveTypeCon t
  case rr of
    ResolutionError e -> throwError e
    LocalType d       -> resolveLocalPythonType d
    Imported md d     -> case dataConDef d of
      Nothing -> resolveForeign' d
      Just _  -> do
        modName <- mkModuleCode md
        typeName <- mkTypeCode d
        pure $ importItem $ modName <> (typeName :| [])
    Primitive pt      -> do
      let simple t' = (Set.empty, ESimple t')
          typing t' = importItem ("typing" :| [t'])
      pure $ case pt of
        PUnit   -> simple "object"
        PBool   -> simple "bool"
        PInt32  -> simple "int"
        PDouble -> simple "float"
        PString -> simple "str"
        PMaybe  -> typing "Optional"
        PList   -> typing "List"

resolveLocalPythonType :: GuguguK r m => Data -> m (Set ImportStmt, Expr)
resolveLocalPythonType d@Data{..} = case dataConDef of
  Nothing -> resolveForeign' d
  Just _  -> do
    typeName <- mkTypeCode d
    pure (Set.empty, ESimple typeName)

resolveForeign' :: GuguguK r m => Data -> m (Set ImportStmt, Expr)
resolveForeign' Data{..} = do
  foreignPragma <- case Map.lookup thisTarget dataForeignMap of
    Just v  -> pure v
    Nothing -> throwError $ printf
      "Type %s does not have foreign pragma" dataName
  case NonEmpty.nonEmpty $ splitOn' "." foreignPragma of
    Nothing -> throwError $ printf "Bad FOREIGN pragma: %s" foreignPragma
    Just qn -> pure $ importItem qn

modulePath :: NonEmpty Identifier -> FilePath
modulePath (part1 :| parts) = path <.> "py"
  where
    path = foldl' (\z t -> z </> T.unpack t) (T.unpack part1) parts


-- Name transformers

mkModuleCode :: GuguguK r m => Module -> m (NonEmpty Identifier)
mkModuleCode Module{..} = do
  GuguguPythonOption{..} <- asks toGuguguPythonOption
  withTransformer transModuleCode $ \f ->
    NonEmpty.fromList $ packagePrefix <> [f moduleName]

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
thisTarget = "python"

importAnn :: ImportStmt
importAnn = ImportFrom ("__future__" :| []) ("annotations" :| [])

eCall :: Expr -> [Expr] -> Expr
eCall func args = func `ECall` arg args

noArg :: ArgumentList
noArg = arg []

arg :: [Expr] -> ArgumentList
arg vs = ArgumentList
  { alPositional = vs
  }

importModule :: NonEmpty Identifier -> (Set ImportStmt, Expr)
importModule modName = (Set.singleton (ImportAs modName alias), ESimple alias)
  where alias = mkGuguguImportAlias modName

importItem :: NonEmpty Identifier -> (Set ImportStmt, Expr)
importItem qName = case NonEmpty.nonEmpty $ NonEmpty.init qName of
  Just mod' -> second (`EAttr` NonEmpty.last qName) (importModule mod')
  Nothing   -> (Set.empty, ESimple $ NonEmpty.head qName)

mkGuguguImportAlias :: NonEmpty Identifier -> Identifier
mkGuguguImportAlias ps = "_gugugu_i_" <> T.intercalate "_" (toList ps)

withTransformer :: GuguguK r m
                => (GuguguNameTransformers -> NameTransformer)
                -> ((Text -> Text) -> a)
                -> m a
withTransformer selector k = do
  nt <- asks $ selector . nameTransformers . toGuguguPythonOption
  pure . k $ runNameTransformer nt


-- Instances

instance HasGuguguPythonOption GuguguPythonEnv where
  toGuguguPythonOption = gOpts

instance HasResolutionContext GuguguPythonEnv where
  toResolutionContext = gCtx
