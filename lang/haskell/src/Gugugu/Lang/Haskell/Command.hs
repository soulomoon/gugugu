{-|
Command line entrypoint
 -}
{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Gugugu.Lang.Haskell.Command
  ( guguguHaskellMain
  ) where

import           Data.Foldable
import qualified Data.Map.Strict     as Map
import           Options.Applicative
import           System.FilePath

import           Gugugu.Resolver
import           Gugugu.Utilities

import           Gugugu.Lang.Haskell


-- | The command line entrypoint
guguguHaskellMain :: IO ()
guguguHaskellMain = runExceptIO $ do
  let version = "Gugugu Haskell " <> CURRENT_PACKAGE_VERSION
  GuguguCmdOption{..} <- execParser' optParser version
  modules <- loadAllModules inputDir
  fs <- makeFiles opts modules
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
  derivings' <- strOption $ fold
    [ long "derivings"
    , value ""
    , metavar "DERIVINGS"
    , help $ "deriving clause for data type, use comma to separate multiples, "
          <> "e.g. Eq,Show"
    ]
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
    , derivings     = splitOn' "," derivings'
    , ..
    }
