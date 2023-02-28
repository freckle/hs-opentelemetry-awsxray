module OpenTelemetry.AWSXRay.Propagator
  ( awsXRayContextPropagator
  , awsXRayContextPropagatorOnError
  ) where

import Prelude

import Control.Error.Util (note)
import Network.HTTP.Types.Header (HeaderName, RequestHeaders, ResponseHeaders)
import OpenTelemetry.AWSXRay.TraceInfo
import OpenTelemetry.Context
  (Context, insertBaggage, insertSpan, lookupBaggage, lookupSpan)
import OpenTelemetry.Propagator
import OpenTelemetry.Trace.Core (getSpanContext, wrapSpanContext)

awsXRayContextPropagator :: Propagator Context RequestHeaders ResponseHeaders
awsXRayContextPropagator = awsXRayContextPropagatorOnError $ \_ _ -> pure ()

awsXRayContextPropagatorOnError
  :: (RequestHeaders -> String -> IO ())
  -- ^ Called on failure to find or parse an @X-Amzn-TraceId@ header
  -> Propagator Context RequestHeaders ResponseHeaders
awsXRayContextPropagatorOnError onErr = Propagator
  { propagatorNames = ["awsxray trace context"]
  , extractor = \hs c -> do
    case fromXRayHeader =<< note "not found" (lookup hAmznTraceId hs) of
      Left err -> c <$ onErr hs err
      Right TraceInfo {..} -> do
        let wrapped = wrapSpanContext spanContext
        pure $ maybe id insertBaggage baggage $ insertSpan wrapped c
  , injector = \c hs -> case lookupSpan c of
    Nothing -> pure hs
    Just sp -> do
      info <- TraceInfo <$> getSpanContext sp <*> pure (lookupBaggage c)
      pure $ (hAmznTraceId, toXRayHeader info) : hs
  }

hAmznTraceId :: HeaderName
hAmznTraceId = "X-Amzn-Trace-Id"
