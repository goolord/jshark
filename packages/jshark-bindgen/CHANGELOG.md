# Changelog

## 0.1.0.0

* First release. See the [repository changelog](https://github.com/goolord/jshark/blob/master/CHANGELOG.md)
  for the full history of this package and the rest of the monorepo.
* Extractor JSON is schema-versioned and validated on decode. Every
  overload is extracted with a stable identity and emitted under a distinct
  name. `T | null` arguments are unwrapped to native `null`/value via
  `unsafeOptionToNative`; nested nullables are surfaced as
  `unsupported-nullable` diagnostics. TypeScript is pinned to 5.9.3 and
  resolved from `JSHARK_BINDGEN_TYPESCRIPT`, the extractor's directory, or
  the consumer's `node_modules`.
