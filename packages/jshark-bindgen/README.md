# jshark-bindgen

Generate typed Haskell `ffi` wrappers from TypeScript declarations:

```bash
cabal run jshark-bindgen -- lib.d.ts --module JShark.Lib
```

Reads functions, classes, and namespaces from TypeScript `.d.ts` files or
JSDoc and emits Haskell wrappers with concrete type signatures. The extractor
requires Bun and the `typescript` npm package.

## Supported types

| TypeScript | Emitted as | Notes |
|------------|------------|-------|
| `number`, `string`, `boolean`, `bigint`, `void` | `Expr` / `EffectSyntax ()` | primitives |
| `T[]`, `Array<T>`, `ReadonlyArray<T>` | `Expr f ('Array ...)` | element types erased recursively |
| `T \| null` / `T \| undefined` | `Option T` (tagged) | arguments are unwrapped to native `null`/value at the call; returns are tagged with `unsafeNullable` |
| `Promise<T>` | `MutableObject (Promise T)` | `.then`/`.catch` via `JShark.Promise` |
| `Map<K,V>`, `Set<T>` | `'Map` / `'Set` handles | effect-only handles |
| `Uint8Array`, `Uint8ClampedArray` | byte-buffer universes | distinct wrap/clamp |
| class / interface / enum / named type | `'MutableObject <Phantom>` | fields via `Field` instances |
| anything else (`any`, `unknown`, unions) | `JsUnknown` | surfaced as a diagnostic |

## Binding behavior

* **Overloads** get distinct, stable Haskell names (`ms`, `ms2`), one per signature.
* **Optional arguments** are accepted only in trailing position; optional
  parameters are omitted from the generated wrapper (callers that need to
  pass `undefined` explicitly should model it as `T | undefined`).
* **Readonly properties** still generate a `Field` instance and read
  accessor; there is no runtime write barrier, so `set` is available on the
  handle as usual.
* **Nullable inside a container or callback** (`Array<T | null>`,
  `(x: T | null) => void`) is not yet converted at the boundary; it is
  reported as an `unsupported-nullable` diagnostic rather than passed as a
  tagged object to native JS.
* Extractor output is schema-versioned and checked when decoded. TypeScript
  is pinned to `5.9.3`.

Part of the [JShark](https://github.com/goolord/jshark) monorepo.
