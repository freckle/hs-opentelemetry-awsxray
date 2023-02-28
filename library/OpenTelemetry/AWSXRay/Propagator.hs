module OpenTelemetry.AWSXRay.Propagator
  ( awsXRayContextPropagator
  ) where

import Prelude

import Control.Error.Util (note)
import Control.Monad (unless)
import Network.HTTP.Types.Header
  (HeaderName, RequestHeaders, ResponseHeaders, hUserAgent)
import OpenTelemetry.AWSXRay.TraceInfo
import OpenTelemetry.Context
  (Context, insertBaggage, insertSpan, lookupBaggage, lookupSpan)
import OpenTelemetry.Propagator
import OpenTelemetry.Trace.Core (getSpanContext, wrapSpanContext)
import System.IO (hPutStrLn, stderr)

awsXRayContextPropagator :: Propagator Context RequestHeaders ResponseHeaders
awsXRayContextPropagator = awsXRayContextPropagator' $ \hs err ->
  -- Skip logging this on health-check requests
  unless (lookup hUserAgent hs == Just "ELB-HealthChecker/2.0")
    $ hPutStrLn stderr
    $ "[awsXRayContextPropagator] Invalid "
    <> show hAmznTraceId
    <> ": "
    <> err
    <> ", Request Headers: "
    <> show hs

awsXRayContextPropagator'
  :: (RequestHeaders -> String -> IO ())
  -- ^ Called when the extractor fails to parse X-Amzn-TraceId header
  -> Propagator Context RequestHeaders ResponseHeaders
awsXRayContextPropagator' onErr = Propagator
  { propagatorNames = ["awsxray trace context"]
  , extractor = \hs c -> do
    let
      eInfo = do
        h <- note "Header not found" $ lookup hAmznTraceId hs
        fromXRayHeader h

    case eInfo of
      Left err -> c <$ onErr hs err
      Right TraceInfo {..} -> do
        pure
          . maybe id insertBaggage baggage
          . insertSpan (wrapSpanContext spanContext)
          $ c
  , injector = \c hs -> case lookupSpan c of
    Nothing -> pure hs
    Just sp -> do
      info <- TraceInfo <$> getSpanContext sp <*> pure (lookupBaggage c)
      pure $ (hAmznTraceId, toXRayHeader info) : hs
  }

hAmznTraceId :: HeaderName
hAmznTraceId = "X-Amzn-Trace-Id"
