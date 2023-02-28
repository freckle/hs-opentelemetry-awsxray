-- | Lenses for modifying a 'TracerProviderOptions'
module OpenTelemetry.Trace.Setup.Lens
  ( idGeneratorL
  , samplerL
  , resourcesL
  , attributeLimitsL
  , spanLimitsL
  , propagatorL
  , loggerL
  ) where

import Prelude

import Data.Text (Text)
import Lens.Micro (Lens', lens)
import Network.HTTP.Types.Header (RequestHeaders, ResponseHeaders)
import OpenTelemetry.Attributes (AttributeLimits)
import OpenTelemetry.Context (Context)
import OpenTelemetry.Logging.Core (Log)
import OpenTelemetry.Propagator (Propagator)
import OpenTelemetry.Resource (MaterializedResources)
import OpenTelemetry.Trace (TracerProviderOptions(..))
import OpenTelemetry.Trace.Core (SpanLimits)
import OpenTelemetry.Trace.Id.Generator (IdGenerator)
import OpenTelemetry.Trace.Sampler (Sampler)

idGeneratorL :: Lens' TracerProviderOptions IdGenerator
idGeneratorL = lens tracerProviderOptionsIdGenerator
  $ \x y -> x { tracerProviderOptionsIdGenerator = y }

samplerL :: Lens' TracerProviderOptions Sampler
samplerL = lens tracerProviderOptionsSampler
  $ \x y -> x { tracerProviderOptionsSampler = y }

resourcesL :: Lens' TracerProviderOptions MaterializedResources
resourcesL = lens tracerProviderOptionsResources
  $ \x y -> x { tracerProviderOptionsResources = y }

attributeLimitsL :: Lens' TracerProviderOptions AttributeLimits
attributeLimitsL = lens tracerProviderOptionsAttributeLimits
  $ \x y -> x { tracerProviderOptionsAttributeLimits = y }

spanLimitsL :: Lens' TracerProviderOptions SpanLimits
spanLimitsL = lens tracerProviderOptionsSpanLimits
  $ \x y -> x { tracerProviderOptionsSpanLimits = y }

propagatorL
  :: Lens'
       TracerProviderOptions
       (Propagator Context RequestHeaders ResponseHeaders)
propagatorL = lens tracerProviderOptionsPropagators
  $ \x y -> x { tracerProviderOptionsPropagators = y }

loggerL :: Lens' TracerProviderOptions (Log Text -> IO ())
loggerL = lens tracerProviderOptionsLogger
  $ \x y -> x { tracerProviderOptionsLogger = y }
