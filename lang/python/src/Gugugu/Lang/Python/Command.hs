{-|
Command line entrypoint
 -}
{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Gugugu.Lang.Python.Command
  ( guguguPythonMain
  ) where

import           Data.Foldable
import qualified Data.Map.Strict     as Map
import           Options.Applicative
import           System.FilePath

import           Gugugu.Resolver
import           Gugugu.Utilities

import           Gugugu.Lang.Python


-- | The command line entrypoint
guguguPythonMain :: IO ()
guguguPythonMain = runExceptIO $ do
  let version = "Gugugu Python " <> CURRENT_PACKAGE_VERSION
  GuguguCmdOption{..} <- execParser' optParser version
  modules <- loadAllModules inputDir
  fs <- makeFiles opts modules
  for_ (Map.toList fs) $ \(p, sf) ->
    writeSrcCompToFile (outputDir </> p) sf


optParser :: Parser GuguguPythonOption
optParser = do
  packagePrefix' <- strOption $ fold
    [ long "package-prefix"
    , short 'p'
    , metavar "PACKAGE_PREFIX"
    , help "the package prefix, e.g. some.package.prefix"
    ]
  nameTransformers <- guguguNameTransformers GuguguNameTransformers
    { transModuleCode  = ToLower
    , transModuleValue = ToSnake
    , transModuleType  = NoTransform
    , transFuncCode    = ToSnake
    , transFuncValue   = ToSnake
    , transTypeCode    = NoTransform
    , transFieldCode   = ToSnake
    , transFieldValue  = ToSnake
    , transEnumCode    = ToUpperSnake
    , transEnumValue   = ToUpperSnake
    }
  pure GuguguPythonOption
    { packagePrefix = splitOn' "." packagePrefix'
    , ..
    }
