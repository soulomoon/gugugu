{-|
Utilities to create source file.
 -}
{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module Gugugu.Utilities.Command
  ( runExceptIO
  , splitOn'
  , GuguguCmdOption(..)
  , execParser'
  , pWithCodec

  , GuguguNameTransformers(..)
  , guguguNameTransformers
  , nameTransformerOption
  ) where

import           Control.Monad.Except
import           Data.Foldable
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Options.Applicative
import           System.Exit
import           System.IO
import           Text.PrettyPrint.ANSI.Leijen

import           Gugugu.Utilities.SourceUtils


-- | Print error and exit on error.
runExceptIO :: ExceptT String IO a -> IO a
runExceptIO eio = do
  ea <- runExceptT eio
  case ea of
    Left e  -> do
      hPutStrLn stderr e
      exitFailure
    Right a -> pure a

-- | Like 'T.splitOn' but return empty list instead of a list of empty string.
splitOn' :: Text -> Text -> [Text]
splitOn' delim t = if T.null t then [] else T.splitOn delim t

-- | Common gugugu option fields
data GuguguCmdOption a
  = GuguguCmdOption
    { inputDir  :: FilePath   -- ^ Input directory
    , outputDir :: FilePath   -- ^ Output directory
    , opts      :: a          -- ^ Other options
    }
  deriving Show

-- | Execute the parser
execParser' :: MonadIO m => Parser a -> m (GuguguCmdOption a)
execParser' parser = liftIO $ execParser infoParser
  where
    infoParser = info fullParser fullDesc
    fullParser = cmdParser <**> helper <**> pHelpTransformer
    cmdParser  = mkCmdOptParser parser

-- | Parser for whether generate codec
pWithCodec :: Parser Bool
pWithCodec = switch $ fold
  [ long "with-codec"
  , help "pass this flag to generate codecs, default to false"
  ]


data GuguguNameTransformers
  = GuguguNameTransformers
    { transModuleCode :: NameTransformer    -- ^ Module name code
    , transTypeCode   :: NameTransformer    -- ^ Type name code
    , transFieldCode  :: NameTransformer    -- ^ Field name code
    , transFieldValue :: NameTransformer    -- ^ Field name value
    }
  deriving Show


-- | Get all transformers
guguguNameTransformers :: GuguguNameTransformers
                       -> Parser GuguguNameTransformers
guguguNameTransformers defaults = do
  transModuleCode' <- nameTransformerOption $ fold
    [ long "trans-module-code"
    , help "module name transformer for code"
    , value $ transModuleCode defaults
    ]
  transTypeCode' <- nameTransformerOption $ fold
    [ long "trans-type-code"
    , help "type name transformer for code"
    , value $ transTypeCode defaults
    ]
  transFieldCode' <- nameTransformerOption $ fold
    [ long "trans-field-code"
    , help "record field name transformer for code"
    , value $ transFieldCode defaults
    ]
  transFieldValue' <- nameTransformerOption $ fold
    [ long "trans-field-value"
    , help "record field name transformer for value"
    , value $ transFieldValue defaults
    ]
  pure GuguguNameTransformers
    { transModuleCode = transModuleCode'
    , transTypeCode   = transTypeCode'
    , transFieldCode  = transFieldCode'
    , transFieldValue = transFieldValue'
    }

-- | Make Parser for 'NameTransformer'
nameTransformerOption :: Mod OptionFields NameTransformer
                      -> Parser NameTransformer
nameTransformerOption opts = option readNameTransformer $ fold
  [ value NoTransform
  , showDefaultWith nameTransformerToString
  , help ""
  ] <> opts


mkCmdOptParser :: Parser a -> Parser (GuguguCmdOption a)
mkCmdOptParser optParser = do
  inputDir <- strOption $ fold
    [ long "input"
    , short 'i'
    , metavar "INPUT"
    , help "the directory containing the definition files"
    ]
  outputDir <- strOption $ fold
    [ long "output"
    , short 'o'
    , metavar "OUTPUT"
    , help "the directory to put the generated sources"
    ]
  opts <- optParser
  pure GuguguCmdOption{..}

pHelpTransformer :: Parser (a -> a)
pHelpTransformer = infoOption (show transformerHelpDoc) $ fold
  [ long "help-transformers"
  , help "list available name transformers"
  , hidden
  ]

transformerHelpDoc :: Doc
transformerHelpDoc =
  let makeEntry nt       = fillBreak 30 (text $ nameTransformerToString nt)
                       <+> makeDescription nt
      makeDescription nt = text $ case nt of
        NoTransform  -> "do no transformation"
        ToSnake      -> "convert to snake_case"
        ToUpperSnake -> "convert to UPPER_SNAKE_CASE"
        ToLower      -> "convert to alllowercase"
      entries            = vcat $ fmap makeEntry allTransformers
  in text "Available name transformers are:" <$$> indent 2 entries

readNameTransformer :: ReadM NameTransformer
readNameTransformer = do
  s <- str
  case find ((== s) . nameTransformerToString) allTransformers of
    Just nt -> pure nt
    Nothing -> readerError $
         "unknown name transformer: " <> s
      <> ", list available transformers with --help-transformers"

nameTransformerToString :: NameTransformer -> String
nameTransformerToString nt = case nt of
  NoTransform  -> "id"
  ToSnake      -> "snake"
  ToUpperSnake -> "upper-snake"
  ToLower      -> "lower"

allTransformers :: [NameTransformer]
allTransformers = [ NoTransform
                  , ToSnake
                  , ToUpperSnake
                  , ToLower
                  ]
