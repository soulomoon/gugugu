{-|
Command line entrypoint
 -}
{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Gugugu.Lang.Typescript.Command
  ( guguguTypescriptMain
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B
import           Data.FileEmbed
import           Data.Foldable
import qualified Data.Map.Strict        as Map
import           Options.Applicative
import           System.Directory
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
  liftIO $ do
    let writeRuntimeFile name content = do
          createDirectoryIfMissing True runtimeDir
          let path = runtimeDir </> (name <> ".ts")
          putStrLn $ "Writing file: " <> path
          B.writeFile path content
        runtimeDir = outputDir </> "gugugu"
    when (withCodec opts) $
      writeRuntimeFile "codec" codecFile
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
  withCodec <- pWithCodec
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


-- Embeded runtime files

codecFile :: ByteString
codecFile = $(embedFile "runtime/codec.ts")
