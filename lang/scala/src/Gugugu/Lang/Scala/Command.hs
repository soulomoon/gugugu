{-|
Command line entrypoint
 -}
{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Gugugu.Lang.Scala.Command
  ( guguguScalaMain
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B
import           Data.FileEmbed
import           Data.Foldable
import qualified Data.Map.Strict        as Map
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           Options.Applicative
import           System.Directory
import           System.FilePath
import           System.IO

import           Gugugu.Resolver
import           Gugugu.Utilities

import           Gugugu.Lang.Scala


-- | The command line entrypoint
guguguScalaMain :: IO ()
guguguScalaMain = runExceptIO $ do
  GuguguCmdOption{..} <- execParser' optParser
  modules <- loadAllModules inputDir
  fs <- makeFiles opts modules
  liftIO $ do
    let writeRuntimeFiles pkg files = do
          let fullPkg = runtimePkg opts <> [pkg]
              pkgDir  = foldl' (\x y -> x </> T.unpack y) outputDir fullPkg
          createDirectoryIfMissing True pkgDir
          for_ files $ \(filename, content) -> do
            let path = pkgDir </> filename
            putStrLn $ "Writing file: " <> path
            withFile path WriteMode $ \h -> do
              T.hPutStr h $ "package " <> T.intercalate "." fullPkg <> "\n\n"
              B.hPut h content
    when (withCodec opts) $
      writeRuntimeFiles "codec" codecFiles
  for_ (Map.toList fs) $ \(p, sf) ->
    writeSrcCompToFile (outputDir </> p) sf


optParser :: Parser GuguguScalaOption
optParser = do
  packagePrefix' <- strOption $ fold
    [ long "package-prefix"
    , short 'p'
    , metavar "PACKAGE_PREFIX"
    , help "the package prefix, e.g. com.example.foo.generated"
    ]
  runtimePkg' <- strOption $ fold
    [ long "runtime-package"
    , short 'r'
    , value "gugugu.lang.scala.runtime"
    , showDefault
    , metavar "RUNTIME_PACKAGE"
    , help "location of gugugu runtime package"
    ]
  withCodec <- pWithCodec
  nameTransformers <- guguguNameTransformers GuguguNameTransformers
    { transModuleCode = ToLower
    , transTypeCode   = NoTransform
    , transFieldCode  = NoTransform
    , transFieldValue = ToSnake
    }
  pure GuguguScalaOption
    { packagePrefix = splitOn' "." packagePrefix'
    , runtimePkg    = splitOn' "." runtimePkg'
    , ..
    }


-- Embeded runtime files

codecFiles :: [(FilePath, ByteString)]
codecFiles = $(embedDir "runtime/codec")
