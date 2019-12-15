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

import           Control.Concurrent
import           Control.Exception
import           Control.Monad.Cont
import           Data.Aeson
import           Data.ByteString                       (ByteString)
import qualified Data.ByteString                       as B
import qualified Data.ByteString.Lazy                  as BL
import           Data.Foldable                         hiding (fold)
import           Data.Function
import qualified Data.Map.Strict                       as Map
import           Data.String
import qualified Data.Text                             as T
import           Data.Text.Encoding
import           Data.Time
import qualified Data.Vector                           as V
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import           System.CPUTime
import           System.IO
import           Text.Printf

import           Gugugu.Lang.Haskell.Runtime.Transport
import           GuguguExamples.Codec.Json
import           GuguguExamples.Definitions.Hello
import           GuguguExamples.Definitions.HelloTypes
import           GuguguExamples.JsonHttp
import           GuguguExamples.Utils

main :: IO ()
main = runWithHostAndPort $ \host port -> do
  let settings                           = defaultSettings
        & setPort port
        & setHost (fromString host)
        & setInstallShutdownHandler installShutdownHandler
      installShutdownHandler closeSocket = void $ forkIO $ do
        let waitEof = do
              b <- B.hGetSome stdin 4096
              if B.null b then pure () else waitEof
        putStrLn "Press Ctrl-D to shut down the server"
        waitEof
        putStrLn "Shutting down the server"
        closeSocket
  putStrLn "Server is about to start"
  runSettings settings helloApp

helloApp :: Application
helloApp req respond = runHandlerIO' respond $ case pathInfo req of
  [] -> notFound'
  ps -> do
    let qn = QualName (V.fromList (init ps)) (last ps)
    case helloTransport qn codecHandler of
      Nothing -> notFound'
      Just k  -> do
        reqBody <- liftIO $ strictRequestBody req
        case decode' reqBody of
          Nothing -> badRequest'
          Just ra -> do
            let meta = headersToMeta $ requestHeaders req
            WithMeta rmeta rb <- k $ WithMeta meta ra
            let payload  = encode rb
                headers' = metaToHeaders rmeta
                headers  = ("Content-Length", cl                ) :
                           ("Content-Type"  , "application/json") :
                           headers'
                cl       = encodeUtf8 $ T.pack $ show $ BL.length payload
            completeWith $ responseLBS ok200 headers payload

newtype HandlerIO a = HandlerIO
  { runHandlerIO :: forall r. (a -> IO r) -> (Response -> IO r) -> IO r }

type HandlerIO' = forall a. HandlerIO a

runHandlerIO' :: (Response -> IO a) -> HandlerIO' -> IO a
runHandlerIO' respond h = runHandlerIO h id respond

completeWith :: Response -> HandlerIO'
completeWith resp = HandlerIO $ \_ respond -> respond resp

badRequest :: String -> HandlerIO'
badRequest reason = completeWith $ responseLBS badRequest400 [header] BL.empty
  where header = ("X-Gugugu-Error", stringToBs reason)

badRequest' :: HandlerIO'
badRequest' = completeWith $ responseLBS badRequest400 [] BL.empty

notFound' :: HandlerIO'
notFound' = completeWith $ responseLBS notFound404 [] BL.empty

stringToBs :: String -> ByteString
stringToBs = encodeUtf8 . T.pack

data HelloApp = HelloApp

helloTransport :: ServerTransport' WithMeta HandlerIO Value CodecF
helloTransport = mkHelloTransport' JsonCodecImpl HelloApp

codecHandler :: ServerCodecHandler' WithMeta HandlerIO Value CodecF
codecHandler decodeA encodeB k fr = do
  let WithMeta meta ra = fr
  a <- case decodeA ra of
    Right v -> pure v
    Left e  -> do
      let msg = "Decoding Error: " <> e
      liftIO $ putStrLn msg
      badRequest msg
  WithMeta rmeta b <- k $ WithMeta meta a
  rb <- case encodeB b of
    Right v -> pure v
    Left e  -> do
      let msg = "Encoding Error: " <> e
      liftIO $ putStrLn msg
      badRequest msg
  pure $ WithMeta rmeta rb

withMeta :: (a -> IO b) -> HelloApp -> WithMeta a -> HandlerIO (WithMeta b)
withMeta k _ fa = liftIO $ do
  let WithMeta meta a = fa
  for_ (Map.toList meta) $ \(k', v) -> printf "Metadata: %s %s\n" k' v
  begin <- getCPUTime
  b <- k a
  end <- getCPUTime
  let rmeta = Map.singleton "X-Process-Time" $ T.pack $ printf "%f ms" $
        (fromIntegral (end - begin) / 1000000000 :: Double)
  pure $ WithMeta rmeta b

instance HelloModule HelloApp WithMeta WithMeta HandlerIO where
  fold = withMeta $ \FoldRequest{..} -> evaluate $ case foldRequestOp of
    Add -> foldr (+) foldRequestInitial foldRequestValues
    Mul -> foldr (*) foldRequestInitial foldRequestValues
  calculateFibs = withMeta $ \n ->
    let f (i, x, y) = if i >= n then Nothing else Just (v, next)
          where v    = AssociatedListEntry
                  { associatedListEntryIndex = i
                  , associatedListEntryValue = y
                  }
                next = (i + 1, y, x + y)
    in evaluate $ AssociatedList $ V.unfoldr f (0, 0, 1)
  incrOneDay = withMeta $
    evaluate . utcToLocalTime utc . addUTCTime nominalDay . localTimeToUTC utc


instance Functor HandlerIO where
  fmap f h = HandlerIO $ \k respond -> runHandlerIO h (k . f) respond

instance Applicative HandlerIO where
  pure a = HandlerIO $ \k _ -> k a
  hf <*> ha = HandlerIO $ \k respond ->
    runHandlerIO hf (\f -> runHandlerIO ha (k . f) respond) respond

instance Monad HandlerIO where
  ha >>= f = HandlerIO $ \k respond ->
    runHandlerIO ha (\a -> runHandlerIO (f a) k respond) respond

instance MonadIO HandlerIO where
  liftIO io = HandlerIO $ \k _ -> io >>= k
