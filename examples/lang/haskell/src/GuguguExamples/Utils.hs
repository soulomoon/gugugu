{-# OPTIONS_GHC -Wall -Werror #-}
module GuguguExamples.Utils
  ( runWithHostAndPort
  ) where

import           System.Environment
import           System.IO


runWithHostAndPort :: (String -> Int -> IO a) -> IO a
runWithHostAndPort k = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  maybeHost <- lookupEnv "GUGUGU_EXAMPLE_HOST"
  maybePort <- lookupEnv "GUGUGU_EXAMPLE_PORT"
  let host = case maybeHost of
        Just v  -> v
        Nothing -> "127.0.0.1"
      port = case maybePort of
        Just v  -> read v
        Nothing -> 8080
  k host port
