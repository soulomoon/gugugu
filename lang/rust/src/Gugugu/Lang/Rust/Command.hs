{-|
Command line entrypoint
 -}
{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Gugugu.Lang.Rust.Command
  ( guguguRustMain
  ) where

import           Data.Foldable
import qualified Data.Map.Strict     as Map
import           Options.Applicative
import           System.FilePath

import           Gugugu.Resolver
import           Gugugu.Utilities

import           Gugugu.Lang.Rust


-- | The command line entrypoint
guguguRustMain :: IO ()
guguguRustMain = runExceptIO $ do
  let version = "Gugugu Rust " <> CURRENT_PACKAGE_VERSION
  GuguguCmdOption{..} <- execParser' optParser version
  modules <- loadAllModules inputDir
  fs <- makeFiles opts modules
  for_ (Map.toList fs) $ \(p, sf) ->
    writeSrcCompToFile (outputDir </> p) sf


optParser :: Parser GuguguRustOption
optParser = do
  modulePrefix' <- strOption $ fold
    [ long "module-prefix"
    , short 'p'
    , metavar "MODULE_PREFIX"
    , help "the package prefix, e.g. some::prefix"
    ]
  runtimeMod' <- strOption $ fold
    [ long "runtime-module"
    , short 'r'
    , value "gugugu::lang::rust::runtime"
    , showDefault
    , metavar "RUNTIME_MODULE"
    , help "location of gugugu runtime module"
    ]
  derivings' <- strOption $ fold
    [ long "derives"
    , value ""
    , metavar "DERIVES"
    , help $ "derive attribute for data type, "
          <> "use comma to separate multiples, "
          <> "e.g. Debug,PartialEq"
    ]
  nameTransformers <- guguguNameTransformers GuguguNameTransformers
    { transModuleCode  = ToLower
    , transModuleValue = ToSnake
    , transModuleType  = NoTransform
    , transFuncCode    = ToSnake
    , transFuncValue   = ToSnake
    , transTypeCode    = NoTransform
    , transTypeFunc    = ToSnake
    , transFieldCode   = ToSnake
    , transFieldValue  = ToSnake
    , transEnumCode    = NoTransform
    , transEnumValue   = ToUpperSnake
    }
  pure GuguguRustOption
    { modulePrefix = splitOn' "::" modulePrefix'
    , runtimeMod   = splitOn' "::" runtimeMod'
    , derivings    = splitOn' "," derivings'
    , ..
    }
