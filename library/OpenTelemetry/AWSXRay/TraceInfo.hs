{-# LANGUAGE NamedFieldPuns #-}

module OpenTelemetry.AWSXRay.TraceInfo
  ( TraceInfo(..)
  , fromXRayHeader
  , toXRayHeader
  ) where

import Prelude

import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import OpenTelemetry.Baggage
import OpenTelemetry.Trace (SpanContext(..))
import OpenTelemetry.Trace.Core
  (defaultTraceFlags, isSampled, setSampled, unsetSampled)
import OpenTelemetry.Trace.Id
  ( Base(..)
  , baseEncodedToSpanId
  , baseEncodedToTraceId
  , spanIdBaseEncodedByteString
  , traceIdBaseEncodedByteString
  )
import qualified OpenTelemetry.Trace.TraceState as TS

-- | The data to read/write from the @X-Amzn-TraceId@ header
data TraceInfo = TraceInfo
  { spanContext :: SpanContext
  , baggage :: Maybe Baggage
  }
  deriving stock Show

-- | @Root=1-{epoch}-{unique}[;Parent={spanId}][;Sampled={1|0}][;meta=attr...]@
fromXRayHeader :: ByteString -> Either String TraceInfo
fromXRayHeader bs = do
  kv <- bsToKeyValues bs

  root <- note "Root not present" $ lookup "Root" kv
  traceId <- case bsSplitOn '-' root of
    ["1", epoch, unique] -> do
        -- epoch can be "at most" 8 hex and we should zero-pad the
        -- short values. NB. This relies on hex being 1 char per char
      let
        epochUnique = bsLeftPad 8 '0' epoch <> unique
        errorPrefix =
          "Root epoch+unique ("
            <> show epochUnique
            <> ") is not a valid TraceId"
      prefix errorPrefix $ baseEncodedToTraceId Base16 epochUnique
    _ -> Left "Splitting on - did not produce exactly 3 parts"

  parent <- note "Parent not present" $ lookup "Parent" kv
  spanId <- prefix "Parent is not a valid SpanId"
    $ baseEncodedToSpanId Base16 parent

  let
    sampled = (== Just "1") $ lookup "Sampled" kv

    traceFlags =
      (if sampled then setSampled else unsetSampled) defaultTraceFlags

    baggage = hush $ decodeBaggageHeader $ bsFromKeyValues $ filter
      ((`notElem` ["Root", "Parent", "Sampled"]) . fst)
      kv

  pure $ TraceInfo
    { spanContext = SpanContext
      { traceFlags
      , traceId
      , spanId
      , isRemote = True
      , traceState = TS.empty
      }
    , baggage
    }

toXRayHeader :: TraceInfo -> ByteString
toXRayHeader TraceInfo { spanContext, baggage } =
  bsFromKeyValues
      [ ("Root", "1-" <> epoch <> "-" <> unique)
      , ("Parent", spanIdBaseEncodedByteString Base16 spanId)
      , ("Sampled", if isSampled traceFlags then "1" else "0")
      ]
    <> maybe "" ((";" <>) . encodeBaggageHeader) baggage
 where
  SpanContext { traceId, spanId, traceFlags } = spanContext
  (epoch, unique) = BS.splitAt 8 $ traceIdBaseEncodedByteString Base16 traceId

hush :: Either e a -> Maybe a
hush = either (const Nothing) Just

note :: e -> Maybe a -> Either e a
note e = maybe (Left e) Right

prefix :: String -> Either String a -> Either String a
prefix p = first (\e -> p <> ": " <> e)

bsToKeyValues :: ByteString -> Either String [(ByteString, ByteString)]
bsToKeyValues = traverse go . bsSplitOn ';'
 where
  go bs = case bsSplitOn '=' bs of
    k : vs -> Right (k, mconcat vs)
    _ -> Left "No = found in key-value piece"

bsFromKeyValues :: [(ByteString, ByteString)] -> ByteString
bsFromKeyValues = BS.intercalate ";" . map (\(k, v) -> k <> "=" <> v)

bsLeftPad :: Int -> Char -> ByteString -> ByteString
bsLeftPad n c bs
  | diff > 0 = BS8.replicate diff c <> bs
  | otherwise = bs
  where diff = BS.length bs - n

bsSplitOn :: Char -> ByteString -> [ByteString]
bsSplitOn c = BS8.splitWith (== c)
