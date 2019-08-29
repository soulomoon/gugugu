{-|
Command line entrypoint
 -}
{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Gugugu.Lang.Scala.Command
  ( guguguScalaMain
  ) where

import           Data.Foldable
import qualified Data.Map.Strict     as Map
import           Options.Applicative
import           System.FilePath

import           Gugugu.Resolver
import           Gugugu.Utilities

import           Gugugu.Lang.Scala


-- | The command line entrypoint
guguguScalaMain :: IO ()
guguguScalaMain = runExceptIO $ do
  GuguguCmdOption{..} <- execParser' optParser
  modules <- loadAllModules inputDir
  fs <- makeFiles opts modules
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
  nameTransformers <- guguguNameTransformers GuguguNameTransformers
    { transModuleCode = ToLower
    , transTypeCode   = NoTransform
    , transFieldCode  = NoTransform
    }
  pure GuguguScalaOption
    { packagePrefix = splitOn' "." packagePrefix'
    , ..
    }
