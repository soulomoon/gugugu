{-|
Resolve the AST
 -}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module Gugugu.Resolver
  (
  -- * Load modules
    loadAllModules
  , Module(..)
  , Data(..)
  , DataCon(..)
  , RecordCon(..)
  , RecordField(..)
  , GType(..)

  -- * Type resolution
  , resolveTypeCon
  , ResolutionContext(..)
  , ResolutionResult(..)
  , PrimitiveType(..)
  , HasResolutionContext(..)
  ) where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Foldable
import           Data.List.NonEmpty   (NonEmpty (..))
import qualified Data.List.NonEmpty   as NonEmpty
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Traversable
import           System.Directory
import           System.FilePath
import           Text.Printf

import qualified Gugugu.Parser        as P


-- | Load all modules in a directory
loadAllModules :: (MonadIO m, MonadError String m) => FilePath -> m [Module]
loadAllModules path = do
  srcs <- liftIO $ findSrcs path
  mds <- for srcs $ \(p, expected) -> do
    modDec <- P.parseModule p
    liftEither $ resolveModuleDec expected modDec
  pure mds


-- | Module
data Module
  = Module
    { moduleName  :: Text
    , moduleDatas :: [Data]
    }
  deriving Show


-- | Data type
data Data
  = Data
    { dataName   :: Text
    , dataConDef :: DataCon
    }
  deriving Show

-- | Data constructor
data DataCon
  = DRecord RecordCon
  deriving Show


-- | Record consturctor
data RecordCon
  = RecordCon
    { recordConName   :: Text
    , recordConFields :: [RecordField]
    }
  deriving Show

-- | Record field
data RecordField
  = RecordField
    { recordFieldName :: Text
    , recordFieldType :: GType
    }
  deriving Show


-- | Gugugu Type
data GType
  = GApp
    { typeCon    :: Text
    , typeParams :: [GType]
    }
  deriving Show


-- | Resolve the type constructor with the context
resolveTypeCon :: (HasResolutionContext r, MonadReader r m)
               => Text -> m ResolutionResult
resolveTypeCon name = do
  rc <- asks toResolutionContext
  let result = case lookupLocal rc name of
        Just v  -> LocalType v
        Nothing -> prim
      prim   = case lookupPrimitives name of
        Just v  -> Primitive v
        Nothing -> notFound
      notFound = ResolutionError $ "cannot resolve type: " <> T.unpack name
  pure result

-- | Resolution context
data ResolutionContext
  = ResolutionContext
    { rcModules       :: Map Text Module
    , rcCurrentModule :: Module
    }
  deriving Show

-- | Resolution result
data ResolutionResult
  = ResolutionError String
  | LocalType Data
  | Primitive PrimitiveType
  deriving Show

-- | Built-in primitive types
data PrimitiveType
  = PUnit     -- ^ Like Haskell @()@ type
  | PBool     -- ^ Boolean type
  | PInt32    -- ^ 32-bit signed integer
  | PDouble   -- ^ Double precision floating point number
  | PString   -- ^ A sequence of characters
  | PMaybe    -- ^ Like Haskell 'Maybe'
  | PList     -- ^ An ordered list of other values
  deriving Show

-- | Utilities to use MonadReader
class HasResolutionContext a where
  -- | Convert @a@ to 'ResolutionContext'
  toResolutionContext :: a -> ResolutionContext


resolveModuleDec :: NonEmpty Text -> P.ModuleDec -> Either String Module
resolveModuleDec expected P.ModuleDec{..} = do
  when ((moduleDecName :| []) /= expected) $ Left $
    printf "Expected: %s\nSaw     : %s"
      (T.intercalate "." $ toList expected)
      moduleDecName
  let go :: [P.Dec] -> Either String Module
      go decs = case decs of
        P.DData dd : decs' -> do
          d <- resolveDataDec dd
          (\m@Module{..} -> m{ moduleDatas = d : moduleDatas }) <$> go decs'
        []                 -> pure Module
          { moduleName  = moduleDecName
          , moduleDatas = []
          }
  go moduleDecBody

resolveDataDec :: P.DataDec -> Either String Data
resolveDataDec P.DataDec{..} = do
  dataConDef <- resolveDataConDef dataDecDef
  case dataConDef of
    DRecord RecordCon{..} | dataDecName /= recordConName ->
      Left "unmatched type name and data constructor name"
    _                                                    -> pure ()
  pure Data{ dataName = dataDecName, .. }

resolveDataConDef :: P.DataCon -> Either String DataCon
resolveDataConDef dc = case dc of
  P.DRecord P.RecordCon{..} -> do
    recordConFields' <- traverse resolveRecordField recordConFields
    pure $ DRecord RecordCon{ recordConFields = recordConFields', .. }

resolveRecordField :: P.RecordField -> Either String RecordField
resolveRecordField P.RecordField{..} = do
  recordFieldType' <- resolveTypeExpr recordFieldType
  pure RecordField{ recordFieldType = recordFieldType', .. }


resolveTypeExpr :: P.TypeExpr -> Either String GType
resolveTypeExpr P.TypeExpr{..} = do
  typeParams <- traverse resolveTypeExpr typeExprParams
  pure GApp{ typeCon = typeExprFirst, .. }


findSrcs :: FilePath -> IO [(FilePath, NonEmpty Text)]
findSrcs path = do
  isDir <- doesDirectoryExist path
  if isDir
    then do
      ps <- listDirectory path
      fmap concat $ traverse (findSrcs' []) $ fmap (path </>) ps
    else pure []

findSrcs' :: [Text] -> FilePath -> IO [(FilePath, NonEmpty Text)]
findSrcs' prefix path = do
  isDir <- doesDirectoryExist path
  if isDir
    then do
      let newPrefix = prefix <> [ T.pack $ takeFileName path ]
      ps <- listDirectory path
      fmap concat $ traverse (findSrcs' newPrefix) $ fmap (path </>) ps
    else
      if takeExtension path == ".pg"
        then
          let lastPart = T.pack $ takeBaseName path
              expected = NonEmpty.fromList $ prefix <> [ lastPart ]
          in pure [(path, expected)]
        else pure []


lookupLocal :: ResolutionContext -> Text -> Maybe Data
lookupLocal ResolutionContext{ rcCurrentModule = md } = lookupModule md

lookupModule :: Module -> Text -> Maybe Data
lookupModule Module{..} name = find ((== name) . dataName) moduleDatas

lookupPrimitives :: Text -> Maybe PrimitiveType
lookupPrimitives name = Map.lookup name primitiveTypes

primitiveTypes :: Map Text PrimitiveType
primitiveTypes = Map.fromList
  [ ("Unit"   , PUnit)
  , ("Bool"   , PBool)
  , ("Int32"  , PInt32)
  , ("Double" , PDouble)
  , ("String" , PString)
  , ("Maybe"  , PMaybe)
  , ("List"   , PList)
  ]
