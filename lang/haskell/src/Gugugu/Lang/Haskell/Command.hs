{-|
Command line entrypoint
 -}
{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Gugugu.Lang.Haskell.Command
  ( guguguHaskellMain
  ) where

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

import           Gugugu.Lang.Haskell


-- | The command line entrypoint
guguguHaskellMain :: IO ()
guguguHaskellMain = runExceptIO $ do
  let version = "Gugugu Haskell " <> CURRENT_PACKAGE_VERSION
  GuguguCmdOption{..} <- execParser' optParser version
  modules <- loadAllModules inputDir
  allFiles <- makeFiles opts modules
  let (maybeCodecFile, fs)  = Map.updateLookupWithKey (\_ _ -> Nothing)
                              codecPath fs'
      (maybeTransFile, fs') = Map.updateLookupWithKey (\_ _ -> Nothing)
                              transPath allFiles
      codecPath             = runtimeFile "Codec"
      transPath             = runtimeFile "Transport"
      runtimeFile name      = foldr (\x z -> T.unpack x </> z)
                              (name <.> "hs") (runtimeMod opts)
  liftIO $ do
    let writePartial path embeded maybePart = for_ maybePart $ \part -> do
          let fullPath = outputDir </> path
          createDirectoryIfMissing True $ takeDirectory fullPath
          putStrLn $ "Writing file: " <> fullPath
          withFile fullPath WriteMode $ \h -> do
            writeWith (T.hPutStr h) part
            T.hPutStr h "\n\n"
            B.hPut h embeded
    writePartial codecPath codecFile maybeCodecFile
    let transFile = f withServer serverFile
                  $ transportCommonFile
          where f p x z = if p opts then z <> "\n\n" <> x else z
    writePartial transPath transFile maybeTransFile
  for_ (Map.toList fs) $ \(p, sf) ->
    writeSrcCompToFile (outputDir </> p) sf


optParser :: Parser GuguguHaskellOption
optParser = do
  packagePrefix' <- strOption $ fold
    [ long "package-prefix"
    , short 'p'
    , metavar "PACKAGE_PREFIX"
    , help "the package prefix, e.g. Some.Package.Prefix"
    ]
  runtimeMod' <- strOption $ fold
    [ long "runtime-module"
    , short 'r'
    , value "Gugugu.Lang.Haskell.Runtime"
    , showDefault
    , metavar "RUNTIME_PACKAGE"
    , help "location of gugugu runtime package"
    ]
  derivings' <- strOption $ fold
    [ long "derivings"
    , value ""
    , metavar "DERIVINGS"
    , help $ "deriving clause for data type, use comma to separate multiples, "
          <> "e.g. Eq,Show"
    ]
  ~(withCodec, withServer, withClient) <- pWithCodecServerClient
  nameTransformers <- guguguNameTransformers GuguguNameTransformers
    { transModuleCode  = NoTransform
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
  pure GuguguHaskellOption
    { packagePrefix = splitOn' "." packagePrefix'
    , runtimeMod    = splitOn' "." runtimeMod'
    , derivings     = splitOn' "," derivings'
    , ..
    }


-- Embeded runtime files

codecFile :: ByteString
codecFile = $(embedFile "runtime/Codec.hs")

transportCommonFile :: ByteString
transportCommonFile = $(embedFile "runtime/Transport.hs")

serverFile :: ByteString
serverFile = $(embedFile "runtime/Server.hs")
