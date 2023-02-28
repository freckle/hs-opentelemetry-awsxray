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

This is mostly a port of the [equivalent][java1] [Java][java2] and [Go][]
projects, and takes inspiration from the existing [Datadog][] modules.

[java1]: https://github.com/open-telemetry/opentelemetry-java-contrib/blob/main/aws-xray/src/main/java/io/opentelemetry/contrib/awsxray/AwsXrayIdGenerator.java
[java2]: https://github.com/open-telemetry/opentelemetry-java-contrib/tree/main/aws-xray-propagator
[go]: https://aws.amazon.com/blogs/opensource/go-support-for-aws-x-ray-now-available-in-aws-distro-for-opentelemetry/
[datadog]: https://github.com/iand675/hs-opentelemetry/tree/main/propagators/datadog

## Usage

The current API exposed by `hs-opentelemetry-sdk` lacks a way to modify the
`TracerProviderOptions` as part of initialization. One's only recourse is to
re-implement the internals of `initializeTracerProvider` so you can do so.

This has been encapsulated in a module of this library that really shouldn't be
in this library:

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
