# jshark-bindgen

Generate typed Haskell `ffi` wrappers from TypeScript declarations:

```bash
cabal run jshark-bindgen -- lib.d.ts --module JShark.Lib
```

Extracts function, class, and namespace declarations from TypeScript `.d.ts`
or JSDoc and emits a Haskell module of monomorphic wrappers. Requires `bun`
with `typescript` installed to run the extractor.

Part of the [JShark](https://github.com/goolord/jshark) monorepo.
