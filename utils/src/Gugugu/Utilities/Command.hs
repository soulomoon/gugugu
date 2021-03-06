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
  , pWithCodecServerClient
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
execParser' :: MonadIO m => Parser a -> String -> m (GuguguCmdOption a)
execParser' parser version = liftIO $ execParser infoParser
  where
    infoParser = info fullParser fullDesc
    fullParser = cmdParser <**> helper <**> pHelpTransformer <**> versionOpt
    cmdParser  = mkCmdOptParser parser
    versionOpt = infoOption version (long "version" <> help "show version")

-- | Parser for whether generate codec \/ server \/ client
pWithCodecServerClient :: Parser (Bool, Bool, Bool)
pWithCodecServerClient = do
  withCodec <- pWithCodec
  withServer <- switch $ fold
    [ long "with-server"
    , help
      "pass this flag to generate server, default to false, implies with-codec"
    ]
  withClient <- switch $ fold
    [ long "with-client"
    , help
      "pass this flag to generate client, default to false, implies with-codec"
    ]
  pure (withCodec || withServer || withClient, withServer, withClient)

-- | Parser for whether generate codec
pWithCodec :: Parser Bool
pWithCodec = switch $ fold
  [ long "with-codec"
  , help "pass this flag to generate codecs, default to false"
  ]


data GuguguNameTransformers
  = GuguguNameTransformers
    { transModuleCode  :: NameTransformer    -- ^ Module name code
    , transModuleValue :: NameTransformer    -- ^ Module name value
    , transModuleType  :: NameTransformer    -- ^ Client/Server code
    , transFuncCode    :: NameTransformer    -- ^ Function name code
    , transFuncValue   :: NameTransformer    -- ^ Function name value
    , transTypeCode    :: NameTransformer    -- ^ Type name code
    , transTypeFunc    :: NameTransformer    -- ^ Type name in function
    , transFieldCode   :: NameTransformer    -- ^ Field name code
    , transFieldValue  :: NameTransformer    -- ^ Field name value
    , transEnumCode    :: NameTransformer    -- ^ Enum name code
    , transEnumValue   :: NameTransformer    -- ^ Enum name value
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
  transModuleValue' <- nameTransformerOption $ fold
    [ long "trans-module-value"
    , help "module name transformer for value"
    , value $ transModuleValue defaults
    ]
  transModuleType' <- nameTransformerOption $ fold
    [ long "trans-module-type"
    , help "module name transformer for type of client/server"
    , value $ transModuleType defaults
    ]
  transFuncCode' <- nameTransformerOption $ fold
    [ long "trans-func-code"
    , help "function name transformer for code"
    , value $ transFuncCode defaults
    ]
  transFuncValue' <- nameTransformerOption $ fold
    [ long "trans-func-value"
    , help "function name transformer for value"
    , value $ transFuncValue defaults
    ]
  transTypeCode' <- nameTransformerOption $ fold
    [ long "trans-type-code"
    , help "type name transformer for code"
    , value $ transTypeCode defaults
    ]
  transTypeFunc' <- nameTransformerOption $ fold
    [ long "trans-type-func"
    , help "type name transformer in function"
    , value $ transTypeFunc defaults
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
  transEnumCode' <- nameTransformerOption $ fold
    [ long "trans-enum-code"
    , help "enum name transformer for code"
    , value $ transEnumCode defaults
    ]
  transEnumValue' <- nameTransformerOption $ fold
    [ long "trans-enum-value"
    , help "enum name transformer for value"
    , value $ transEnumValue defaults
    ]
  pure GuguguNameTransformers
    { transModuleCode  = transModuleCode'
    , transModuleValue = transModuleValue'
    , transModuleType  = transModuleType'
    , transFuncCode    = transFuncCode'
    , transFuncValue   = transFuncValue'
    , transTypeCode    = transTypeCode'
    , transTypeFunc    = transTypeFunc'
    , transFieldCode   = transFieldCode'
    , transFieldValue  = transFieldValue'
    , transEnumCode    = transEnumCode'
    , transEnumValue   = transEnumValue'
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
