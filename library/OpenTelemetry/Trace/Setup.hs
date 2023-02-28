-- | Initialization of a 'TracerProvider' with modified 'TracerProviderOptions'
module OpenTelemetry.Trace.Setup
  ( withTracerProvider
  ) where

import Prelude

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.IO.Unlift (MonadUnliftIO(..))
import OpenTelemetry.Trace
  ( TracerProvider
  , TracerProviderOptions
  , createTracerProvider
  , getTracerProviderInitializationOptions
  , setGlobalTracerProvider
  )
import qualified OpenTelemetry.Trace as Trace
import UnliftIO.Exception (bracket)

withTracerProvider
  :: MonadUnliftIO m
  => (TracerProviderOptions -> TracerProviderOptions)
  -> (TracerProvider -> m a)
  -> m a
withTracerProvider setup =
  bracket (initializeGlobalTracerProvider setup) shutdownTracerProvider

initializeGlobalTracerProvider
  :: MonadIO m
  => (TracerProviderOptions -> TracerProviderOptions)
  -> m TracerProvider
initializeGlobalTracerProvider setup = liftIO $ do
  t <- initializeTracerProvider setup
  t <$ setGlobalTracerProvider t

initializeTracerProvider
  :: MonadIO m
  => (TracerProviderOptions -> TracerProviderOptions)
  -> m TracerProvider
initializeTracerProvider setup = liftIO $ do
  (processors, opts) <- getTracerProviderInitializationOptions
  createTracerProvider processors $ setup opts

shutdownTracerProvider :: MonadIO m => TracerProvider -> m ()
shutdownTracerProvider = liftIO . Trace.shutdownTracerProvider
