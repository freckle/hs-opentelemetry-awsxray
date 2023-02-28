module OpenTelemetry.AWSXRay.TraceInfoSpec
  ( spec
  ) where

import Prelude

import Data.Either (isLeft)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import OpenTelemetry.AWSXRay.TraceInfo
import OpenTelemetry.Baggage (Token)
import qualified OpenTelemetry.Baggage as Baggage
import OpenTelemetry.Trace.Core (isSampled, traceFlags)
import Test.Hspec

spec :: Spec
spec = do
  it "round trips an example" $ do
    let
      h =
        "Root=1-5759e988-bd862e3fe1be46a994272793"
          <> ";Parent=53995c3f42cd8ad8"
          <> ";Sampled=1"

    fmap toXRayHeader (fromXRayHeader h) `shouldBe` Right h

  it "round trips an example with baggage" $ do
    let
      h =
        "Root=1-5759e988-bd862e3fe1be46a994272793"
          <> ";Parent=53995c3f42cd8ad8"
          <> ";Sampled=1"
          <> ";baz=bat" -- order matters due to HashMap
          <> ";foo=bar"

    fmap toXRayHeader (fromXRayHeader h) `shouldBe` Right h

  describe "fromXRayHeader" $ do
    it "requires Parent" $ do
      fromXRayHeader "Root=1-5759e988-bd862e3fe1be46a994272793"
        `shouldSatisfy` isLeft

    context "Sampled" $ do
      it "defaults False" $ do
        let
          eInfo =
            fromXRayHeader
              $ "Root=1-5759e988-bd862e3fe1be46a994272793"
              <> ";Parent=53995c3f42cd8ad8"

        fmap (isSampled . traceFlags . spanContext) eInfo `shouldBe` Right False

      it "interprets =1 as True" $ do
        let
          eInfo =
            fromXRayHeader
              $ "Root=1-5759e988-bd862e3fe1be46a994272793"
              <> ";Parent=53995c3f42cd8ad8"
              <> ";Sampled=1"

        fmap (isSampled . traceFlags . spanContext) eInfo `shouldBe` Right True

      it "interprets other values as as False" $ do
        let
          eInfo =
            fromXRayHeader
              $ "Root=1-5759e988-bd862e3fe1be46a994272793"
              <> ";Parent=53995c3f42cd8ad8"
              <> ";Sampled=flipFlop"

        fmap (isSampled . traceFlags . spanContext) eInfo `shouldBe` Right False

    it "grabs all other values as Baggage" $ do
      let
        eInfo =
          fromXRayHeader
            $ "Root=1-5759e988-bd862e3fe1be46a994272793"
            <> ";Parent=53995c3f42cd8ad8"
            <> ";foo=bar;baz=bat"

      fmap baggage eInfo `shouldBe` Right
        (Just $ Baggage.fromHashMap $ HashMap.fromList
          [ (mkTokenUnsafe "foo", Baggage.element "bar")
          , (mkTokenUnsafe "baz", Baggage.element "bat")
          ]
        )

mkTokenUnsafe :: Text -> Token
mkTokenUnsafe = fromMaybe (error "not a valid Token") . Baggage.mkToken
