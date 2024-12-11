-- | Initialization of a 'TracerProvider' with modified 'TracerProviderOptions'
module OpenTelemetry.Trace.Setup
  ( TracerProviderSetup(..)
  , processorsL
  , optionsL
  , withTracerProvider
  ) where

import Prelude

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.IO.Unlift (MonadUnliftIO(..))
import Lens.Micro (Lens', lens)
import OpenTelemetry.Processor (Processor)
import OpenTelemetry.Trace
  ( TracerProvider
  , TracerProviderOptions
  , createTracerProvider
  , getTracerProviderInitializationOptions
  , setGlobalTracerProvider
  )
import qualified OpenTelemetry.Trace as Trace
import UnliftIO.Exception (bracket)

data TracerProviderSetup = TracerProviderSetup
  { tracerProviderSetupProcessors :: [Processor]
  , tracerProviderSetupOptions :: TracerProviderOptions
  }

tracerProviderSetup
  :: (TracerProviderSetup -> TracerProviderSetup)
  -> ([Processor], TracerProviderOptions)
  -> ([Processor], TracerProviderOptions)
tracerProviderSetup f =
  (tracerProviderSetupProcessors *** tracerProviderSetupOptions)
    . f
    . uncurry TracerProviderSetup

processorsL :: Lens' TracerProviderSetup [Processor]
processorsL = lens tracerProviderSetupProcessors
  $ \x y -> x { tracerProviderSetupProcessors = y }

optionsL :: Lens' TracerProviderSetup [TracerProviderOptions]
optionsL =
  lens tracerProviderSetupOptions $ \x y -> x { tracerProviderSetupOptions = y }

withTracerProvider
  :: MonadUnliftIO m
  => (TracerProviderSetup -> TracerProviderSetup)
  -> (TracerProvider -> m a)
  -> m a
withTracerProvider setup =
  bracket (initializeGlobalTracerProvider setup) shutdownTracerProvider

initializeGlobalTracerProvider
  :: MonadIO m
  => (TracerProviderSetup -> TracerProviderSetup)
  -> m TracerProvider
initializeGlobalTracerProvider setup = liftIO $ do
  t <- initializeTracerProvider setup
  t <$ setGlobalTracerProvider t

initializeTracerProvider
  :: MonadIO m
  => (TracerProviderSetup -> TracerProviderSetup)
  -> m TracerProvider
initializeTracerProvider setup = liftIO $ do
  (processors, opts) <-
    tracerProviderSetup setup <$> getTracerProviderInitializationOptions
  createTracerProvider processors opts

shutdownTracerProvider :: MonadIO m => TracerProvider -> m ()
shutdownTracerProvider = liftIO . Trace.shutdownTracerProvider
