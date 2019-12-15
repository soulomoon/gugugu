{-# OPTIONS_GHC -Wall -Werror #-}
module GuguguExamples.JsonHttp
  ( WithMeta(..)
  , headersToMeta
  , metaToHeaders
  , CodecF
  ) where

import           Data.Bifunctor
import qualified Data.CaseInsensitive as CI
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Text            (Text)
import qualified Data.Text.Encoding   as T
import           Network.HTTP.Types


data WithMeta a = WithMeta (Map Text Text) a deriving Show

headersToMeta :: [Header] -> Map Text Text
headersToMeta headers = Map.fromList $ fmap f headers
  where f = bimap (T.decodeUtf8 . CI.foldedCase) T.decodeUtf8

metaToHeaders :: Map Text Text -> [Header]
metaToHeaders meta = fmap f $ Map.toList meta
  where f = bimap (CI.mk . T.encodeUtf8) T.encodeUtf8

type CodecF = Either String
