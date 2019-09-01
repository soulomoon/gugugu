{-|
Command line entrypoint
 -}
{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Gugugu.Lang.Typescript.Command
  ( guguguTypescriptMain
  ) where

import           Data.Foldable
import qualified Data.Map.Strict        as Map
import           Options.Applicative
import           System.FilePath

import           Gugugu.Resolver
import           Gugugu.Utilities

import           Gugugu.Lang.Typescript


-- | The command line entrypoint
guguguTypescriptMain :: IO ()
guguguTypescriptMain = runExceptIO $ do
  GuguguCmdOption{..} <- execParser' optParser
  modules <- loadAllModules inputDir
  fs <- makeFiles opts modules
  for_ (Map.toList fs) $ \(p, mb) ->
    writeSrcCompToFile (outputDir </> p) mb


optParser :: Parser GuguguTsOption
optParser = do
  packagePrefix' <- strOption $ fold
    [ long "package-prefix"
    , short 'p'
    , metavar "PACKAGE_PREFIX"
    , help "the package prefix, e.g. path/to/generated"
    ]
  nameTransformers <- guguguNameTransformers GuguguNameTransformers
    { transModuleCode = ToLower
    , transTypeCode   = NoTransform
    , transFieldCode  = NoTransform
    , transFieldValue = ToSnake
    }
  pure GuguguTsOption
    { packagePrefix = splitOn' "/" packagePrefix'
    , ..
    }
