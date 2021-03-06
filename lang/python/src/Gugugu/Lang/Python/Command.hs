{-|
Command line entrypoint
 -}
{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Gugugu.Lang.Python.Command
  ( guguguPythonMain
  ) where

import           Control.Monad
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

import           Gugugu.Lang.Python


-- | The command line entrypoint
guguguPythonMain :: IO ()
guguguPythonMain = runExceptIO $ do
  let version = "Gugugu Python " <> CURRENT_PACKAGE_VERSION
  GuguguCmdOption{..} <- execParser' optParser version
  modules <- loadAllModules inputDir
  fs <- makeFiles opts modules
  liftIO $ do
    let writeRuntimeFile name content = do
          let path = pkgDir </> name <.> "py"
          putStrLn $ "Writing file: " <> path
          B.writeFile path content
        pkgDir = foldl' (\x y -> x </> T.unpack y) outputDir $ runtimePkg opts
    when (withCodec opts) $ do
      createDirectoryIfMissing True pkgDir
      writeRuntimeFile "__init__" B.empty
      writeRuntimeFile "codec" codecFile
    when (withServer opts || withClient opts) $ do
      let transportFile = f withClient clientFile . f withServer serverFile
                        $ transportCommonFile
          f p x z       = if p opts then z <> "\n\n" <> x else z
      writeRuntimeFile "transport" transportFile
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
  runtimePkg' <- strOption $ fold
    [ long "runtime-package"
    , short 'r'
    , value "gugugu.lang.python.runtime"
    , showDefault
    , metavar "RUNTIME_PACKAGE"
    , help "location of gugugu runtime package"
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
    , transEnumCode    = ToUpperSnake
    , transEnumValue   = ToUpperSnake
    }
  pure GuguguPythonOption
    { packagePrefix = splitOn' "." packagePrefix'
    , runtimePkg    = splitOn' "." runtimePkg'
    , ..
    }


-- Embeded runtime files

codecFile :: ByteString
codecFile = $(embedFile "runtime/codec.py")

transportCommonFile :: ByteString
transportCommonFile = $(embedFile "runtime/transport.py")

serverFile :: ByteString
serverFile = $(embedFile "runtime/server.py")

clientFile :: ByteString
clientFile = $(embedFile "runtime/client.py")
