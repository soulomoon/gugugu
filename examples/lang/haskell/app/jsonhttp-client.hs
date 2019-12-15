{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
module Main (main) where

import           Control.Exception
import           Data.Aeson
import           Data.Foldable                         hiding (fold)
import qualified Data.Map.Strict                       as Map
import           Data.Time
import           Network.HTTP.Client
import           Network.HTTP.Types
import           Text.Printf

import           Gugugu.Lang.Haskell.Runtime.Transport
import           GuguguExamples.Codec.Json
import           GuguguExamples.Definitions.Hello
import           GuguguExamples.Definitions.HelloTypes
import           GuguguExamples.JsonHttp
import           GuguguExamples.Utils

main :: IO ()
main = withClient $ \client -> do
  let doRequest :: Show b
                => (GuguguClient'' -> WithMeta a -> IO (WithMeta b))
                -> a
                -> IO ()
      doRequest k a = do
        let meta = Map.singleton "X-Some-Meta" "2333"
        WithMeta rmeta b <- k client $ WithMeta meta a
        for_ (Map.toList rmeta) $ \(k', v) -> printf "Metadata: %s = %s\n" k' v
        printf "Got response: %s" (show b)
  doRequest fold FoldRequest
    { foldRequestValues  = [1, 3, 4]
    , foldRequestInitial = 2
    , foldRequestOp      = Add
    }
  doRequest calculateFibs 10
  utcNow <- getCurrentTime
  tz <- getTimeZone utcNow
  let now = utcToLocalTime tz utcNow
  doRequest incrOneDay now

type GuguguClient'' = GuguguClient' WithMeta IO Value CodecF

withClient :: (GuguguClient'' -> IO a) -> IO a
withClient k = runWithHostAndPort $ \host port -> do
  manager <- newManager defaultManagerSettings
  k $ mkGuguguClient' JsonCodecImpl $ \encodeA decodeB qn fa -> do
    let WithMeta meta a = fa
        QualName ns n   = qn
        path            = "/" <> foldr (\x z -> x <> "/" <> z) n ns
        url             = printf "http://%s:%d%s" host port path
        headers'        = metaToHeaders meta
    ra <- case encodeA a of
      Right v -> pure v
      Left e  -> evaluate $ error $ "Encode Error: " <> e
    let payload = encode ra
        headers = ("Content-Type", "application/json") : headers'
    initReq <- parseRequest url
    let req = initReq
          { method         = methodPost
          , requestHeaders = headers
          , requestBody    = RequestBodyLBS payload
          }
    resp <- httpLbs req manager
    b <- case eitherDecode' (responseBody resp) >>= decodeB of
      Left e  -> evaluate $ error $ "Decode Error: " <> e
      Right v -> pure v
    let rmeta = headersToMeta $ responseHeaders resp
    pure $ WithMeta rmeta b
