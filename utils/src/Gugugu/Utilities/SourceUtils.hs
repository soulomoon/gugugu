{-|
Utilities to create source file.
 -}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module Gugugu.Utilities.SourceUtils
  ( SrcComp(..)
  , writeWith
  , SrcCompT
  , writeText
  , indentBy
  , withNewLine
  , doIndent
  , SourceWriterEnv
  , writeSrcCompToFile
  , forWith_
  , forWithComma_
  , NameTransformer(..)
  , runNameTransformer
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Foldable
import qualified Data.List              as List
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           System.Directory
import           System.FilePath
import           System.IO
import           Text.Casing


-- | Utility class to source file
class SrcComp a where
  -- | Write this component with the writer
  writeSrcComp :: Monad m => a -> SrcCompT m ()

-- | Write the component with the write
writeWith :: (SrcComp a, Monad m) => (Text -> m ()) -> a -> m ()
writeWith write a = runReaderT (writeSrcComp a) $
  SourceWriterEnv
    { sweIndentLevel = 0
    , sweWrite       = write
    }

-- | Environment type used by 'writeSrcComp'
type SrcCompT m = ReaderT (SourceWriterEnv m) m

-- | Write 'Text'
writeText :: Monad m => Text -> SrcCompT m ()
writeText t = asks sweWrite >>= \write -> lift $ write t

-- | Increase indent level
indentBy :: Monad m => Int -> SrcCompT m a -> SrcCompT m a
indentBy n = local (\s -> s{ sweIndentLevel = sweIndentLevel s + n })

-- | Do the indentation and line break.
withNewLine :: Monad m => SrcCompT m a -> SrcCompT m a
withNewLine k = doIndent *> k <* writeText "\n"

-- | Do the indentation.
--   Note, you are responsible to ensure that the function is called only once
--   per line.
doIndent :: Monad m => SrcCompT m ()
doIndent = asks sweIndentLevel >>= \n -> writeText (T.replicate n " ")

-- | The environment
data SourceWriterEnv m
  = SourceWriterEnv
    { sweIndentLevel :: Int
    , sweWrite       :: Text -> m ()
    }

-- | Write the component to file, with print to stdout
writeSrcCompToFile :: (MonadIO m, SrcComp a) => FilePath -> a -> m ()
writeSrcCompToFile path sc = liftIO $ do
  createDirectoryIfMissing True $ takeDirectory path
  putStrLn $ "Writing file: " <> path
  withFile path WriteMode $ \h -> writeWith (T.hPutStr h) sc

-- | Like 'for_' but writing a separator between actions
forWith_ :: (Foldable t, Monad m)
         => Text -> t a -> (a -> SrcCompT m b) -> SrcCompT m ()
forWith_ sep as f = sequence_
                  . List.intersperse (writeText sep)
                  . fmap (void . f)
                  . toList
                  $ as

-- | Like 'for_' but writing a comma between actions
forWithComma_ :: (Foldable t, Monad m)
              => t a -> (a -> SrcCompT m b) -> SrcCompT m ()
forWithComma_  = forWith_ ", "

-- | A 'NameTransformer' is just a function of type @'Text' -> 'Text'@.
--   The type is defined for better debug information.
--   The code in Gugugu file are always assumed camelCase or CamelCase
data NameTransformer
  = NoTransform         -- ^ Do no transformation
  | ToSnake             -- ^ convert to snake_case
  | ToUpperSnake        -- ^ convert to SNAKE_CASE
  | ToLower             -- ^ convert to lowercase
  deriving Show

-- | Run the 'NameTransformer'
runNameTransformer :: NameTransformer -> Text -> Text
runNameTransformer nt = case nt of
  NoTransform  -> id
  ToSnake      -> withString $ toQuietSnake . fromHumps
  ToUpperSnake -> withString $ toScreamingSnake . fromHumps
  ToLower      -> T.toLower


withString :: (String -> String) -> Text -> Text
withString f = T.pack . f . T.unpack
