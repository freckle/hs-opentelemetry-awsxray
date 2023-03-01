-- | Functions for dealing with 'Baggage'
--
-- We ported specs about 'Baggage' from the Java Propagator:
--
-- <https://github.com/open-telemetry/opentelemetry-java-contrib/blob/04a2a481934be614e73e41194107ffdd767bc507/aws-xray-propagator/src/test/java/io/opentelemetry/contrib/awsxray/propagator/AwsXrayPropagatorTest.java#L227-L238>
--
-- And using the obvious 'encodeBaggageHeader'/'decodeBaggageHeader' functions
-- fails them. So, we roll our own stuff here.
--
module OpenTelemetry.AWSXRay.Baggage
  ( decode
  , encode
  , module OpenTelemetry.Baggage
  ) where

import Prelude

import Control.Applicative (Alternative)
import Control.Arrow ((***))
import Control.Error.Util (hush)
import Control.Monad (guard)
import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (mapMaybe)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import OpenTelemetry.Baggage (Baggage, Element, Token)
import OpenTelemetry.Baggage as Baggage

-- | Only returns 'Just' if the 'Baggage' is not empty
decode :: [(ByteString, ByteString)] -> Maybe Baggage
decode =
  guarded (/= Baggage.empty)
    . Baggage.fromHashMap
    . HashMap.fromList
    . mapMaybe decodePart
    . filter ((`notElem` nonBaggageKeys) . fst)
  where nonBaggageKeys = ["Root", "Parent", "Sampled"]

decodePart :: (ByteString, ByteString) -> Maybe (Token, Element)
decodePart (bsToken, bsElement) =
  (,)
    <$> (Baggage.mkToken =<< hush (decodeUtf8' bsToken))
    <*> (Baggage.element <$> hush (decodeUtf8' bsElement))

encode :: Baggage -> [(ByteString, ByteString)]
encode = map encodePart . HashMap.toList . Baggage.values

encodePart :: (Token, Element) -> (ByteString, ByteString)
encodePart = Baggage.tokenValue *** encodeUtf8 . Baggage.value

guarded :: Alternative f => (a -> Bool) -> a -> f a
guarded p x = x <$ guard (p x)
