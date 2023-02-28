# AWS X-Ray Support for hs-opentelemetry

<!-- TODO
[![Hackage](https://img.shields.io/hackage/v/hs-opentelemetry-awsxray.svg?style=flat)](https://hackage.haskell.org/package/hs-opentelemetry-awsxray)
[![Stackage Nightly](http://stackage.org/package/hs-opentelemetry-awsxray/badge/nightly)](http://stackage.org/nightly/package/hs-opentelemetry-awsxray)
[![Stackage LTS](http://stackage.org/package/hs-opentelemetry-awsxray/badge/lts)](http://stackage.org/lts/package/hs-opentelemetry-awsxray)
-->

[![CI](https://github.com/freckle/hs-opentelemetry-awsxray/actions/workflows/ci.yml/badge.svg)](https://github.com/freckle/hs-opentelemetry-awsxray/actions/workflows/ci.yml)

An `IdGenerator` and `Propagator` for [`hs-opentelemetry-sdk`][sdk] that
generates and propagates `TraceId` and `SpanId` values that are [compatible with
AWS X-Ray][xray].

[sdk]: https://hackage.haskell.org/package/hs-opentelemetry-sdk
[xray]: https://docs.aws.amazon.com/xray/latest/devguide/xray-api-sendingdata.html#xray-api-traceids

## Usage

The API currently exposed by the `hs-opentelemetry-sdk` package lacks a
convenient way to modify the `TracerProviderOptions` (to tell it to use our
generator/propagator) as part of initialization. You basically have to
re-implement the internals of `initializeTracerProvider` to do so.

This has been encapsulated in a module of this library for convenience:

```hs
import OpenTelemetry.AWSXRay
import OpenTelemetry.Trace
import OpenTelemetry.Trace.Setup
import OpenTelemetry.Trace.Setup.Lens

main :: IO ()
main = do
  withTracerProvider modifyTracerProviderOptions $ \tracerProvider -> do
    let tracer = makeTracer tracerProvider "my-app" tracerOptions

    -- do something with tracer

modifyTracerProviderOptions :: TracerProviderOptions -> TracerProviderOptions
modifyTracerProviderOptions =
  idGeneratorL .~ awsXRayIdGenerator
    . propagatorL <>~ awsXRayContextPropagator
```

This example uses lens, but you certainly don't have to.

## Additional References

- https://docs.aws.amazon.com/xray/latest/devguide/xray-services-adot.html
- https://aws.amazon.com/blogs/opensource/migrating-x-ray-tracing-to-aws-distro-for-opentelemetry/

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)

hacktoberfest: https://hacktoberfest.digitalocean.com/
