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
  withCodec <- pWithCodec
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
