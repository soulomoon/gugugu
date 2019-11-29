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

import           Control.Monad.Except
import           Control.Monad.Reader
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
    , derivings        :: [Text]                  -- ^ derivings clause
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

makeModules :: MonadError String m
            => GuguguHaskellOption
            -> [Module]
            -> m (Map FilePath HaskellModule)
makeModules opts@GuguguHaskellOption{..} modules = do
  let moduleMap = Map.fromList $
        fmap (\md@Module{..} -> (moduleName, md)) modules
  files <- for modules $ \md -> do
    let rCtx = ResolutionContext
          { rcModules       = moduleMap
          , rcCurrentModule = md
          }
        env  = GuguguHaskellEnv
          { gOpts = opts
          , gCtx  = rCtx
          }
    runReaderT (makeModule md) env
  let allFiles = Map.fromList $
        files
  pure allFiles

makeModule :: GuguguK r m
           => Module
           -> m (FilePath, HaskellModule)
makeModule md@Module{..} = do
  importsAndDecs <- traverse makeData moduleDatas
  mod' <- mkModuleCode md
  let (imports, decs) = fold importsAndDecs
  let moduleBody = HaskellModule
        { hmId      = mod'
        , hmImports = Set.toAscList imports
        , hmDecls   = decs
        }
      path       = modulePath mod'
  pure (path, moduleBody)

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


makeType :: GuguguK r m => GType -> m (Set ImportDecl, Type)
makeType GApp{..} = do
  (imports, tFirst) <- resolveHaskellType typeCon
  importsAndParams <- traverse makeType typeParams
  let hsType             = foldl' f (TQCon tFirst) params
      f z t              = case t of
        TApp _ _ -> z `TApp` TParen t
        _        -> z `TApp` t
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

mkFieldCode :: GuguguK r m => Data -> RecordField -> m Text
mkFieldCode Data{..} RecordField{..} = withTransformer transFieldCode $ \f ->
  let withFirst g t = case T.uncons t of
        Nothing      -> t
        Just (c, t') -> T.cons (g c) t'
  in f $ withFirst toLower dataName <> withFirst toUpper recordFieldName

mkEnumCode :: GuguguK r m => Text -> m Text
mkEnumCode name = withTransformer transEnumCode $ \f ->
  f name


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
