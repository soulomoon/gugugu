{-|
Command line entrypoint
 -}
{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Gugugu.Lang.Rust.Command
  ( guguguRustMain
  ) where

import           Control.Monad.IO.Class
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B
import           Data.FileEmbed
import           Data.Foldable
import qualified Data.Map.Strict        as Map
import qualified Data.Text              as T
import           Options.Applicative
import           System.Directory
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
  liftIO $ do
    let writeRuntimeFile name content = do
          let path = outputDir </> runtimePath name
          createDirectoryIfMissing True $ takeDirectory path
          putStrLn $ "Writing file: " <> path
          B.writeFile path content
        runtimePath name              = foldr (\x z -> T.unpack x </> z)
                                        (name <.> "rs") (runtimeMod opts)
    writeRuntimeFile "codec" codecFile
    let transFile = f withClient clientFile . f withServer serverFile
                  $ transportCommonFile
          where f p x z = if p opts then z <> "\n\n" <> x else z
    writeRuntimeFile "transport" transFile
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
  ~(withCodec, withServer, withClient) <- pWithCodecServerClient
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


-- Embeded runtime files

codecFile :: ByteString
codecFile = $(embedFile "runtime/codec.rs")

transportCommonFile :: ByteString
transportCommonFile = $(embedFile "runtime/transport.rs")

serverFile :: ByteString
serverFile = $(embedFile "runtime/server.rs")

clientFile :: ByteString
clientFile = $(embedFile "runtime/client.rs")
