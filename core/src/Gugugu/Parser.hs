{-|
The parser module
 -}
{-# LANGUAGE FlexibleContexts #-}
module Gugugu.Parser
  ( parseModule
  , module Gugugu.Parser.Types
  ) where

import           Control.Monad.Except
import qualified Data.Text.IO         as T

import           Gugugu.Parser.Parser
import           Gugugu.Parser.Types


-- | Parse the sources with give file name
parseModule :: (MonadIO m, MonadError String m) => FilePath -> m ModuleDec
parseModule path = do
  source <- liftIO $ T.readFile path
  let s       = makePState path source
      (em, _) = unP parseModuleDec s
  liftEither em
