{-|
Command line entrypoint
 -}
{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE CPP               #-}
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
  let version = "Gugugu TypeScript " <> CURRENT_PACKAGE_VERSION
  GuguguCmdOption{..} <- execParser' optParser version
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
    when (withServer opts || withClient opts) $ do
      let transportFile = f withServer serverFile . f withClient clientFile
                        $ transportCommonFile
          f p x z       = if p opts then x <> "\n" <> z else z
      writeRuntimeFile "transport" transportFile
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
  ~(withCodec, withServer, withClient) <- pWithCodecServerClient
  nameTransformers <- guguguNameTransformers GuguguNameTransformers
    { transModuleCode  = ToLower
    , transModuleValue = ToSnake
    , transModuleType  = NoTransform
    , transFuncCode    = NoTransform
    , transFuncValue   = ToSnake
    , transTypeCode    = NoTransform
    , transTypeFunc    = NoTransform
    , transFieldCode   = NoTransform
    , transFieldValue  = ToSnake
    , transEnumCode    = NoTransform
    , transEnumValue   = ToUpperSnake
    }
  pure GuguguTsOption
    { packagePrefix = splitOn' "/" packagePrefix'
    , ..
    }


-- Embeded runtime files

codecFile :: ByteString
codecFile = $(embedFile "runtime/codec.ts")

transportCommonFile :: ByteString
transportCommonFile = $(embedFile "runtime/transport.ts")

serverFile :: ByteString
serverFile = $(embedFile "runtime/server.ts")

clientFile :: ByteString
clientFile = $(embedFile "runtime/client.ts")
