# Revision history for jshark

## Unreleased

* `evaluate` is a pure tree walk. Host-language sharing (Haskell
  `let x = e in x + x`) is recovered by `evaluateCached` via
  `StableName` memoization in `IO`.
* Added `JShark.Compiler` to minify generated JS. Default backend is
  esbuild (Closure Compiler and Terser are still selectable).
  `compilePure`/`compileEffect` emit an IIFE so minifiers keep the result,
  and cache outputs in memory (optional on-disk cache, key-verified).
  Named backends throw on failure; `Auto` may fall back and logs to
  stderr. `tryCompileWith` returns `Either`. `nix develop` provides
  `esbuild`. Pass `readableConfig` (or `OutputStyle` `Readable`) for a
  pretty, non-minified snippet with no IIFE.
* Codegen inlines single-use `let`/`Bind` assignments in both readable
  and minified output, so `let x = e in x + 1` becomes `e + 1` while
  `let x = e in x + x` still emits `const n0 = e`.
* Modernized the build: dropped the pinned GHC 8.6.5 / `base < 4.13` bounds
  in favor of a modern GHC (tested with 9.14). The original `quantification`
  dependency was pinned to an unreleased git commit; it (and its `Topaz.Rec`
  types, since split out into a separate `topaz` package) have since been
  properly released to Hackage with permissive bounds, but `topaz-0.8.0.1`
  still transitively requires `hashable <1.5`, which caps `base <4.21`,
  incompatible with GHC 9.14's `base-4.22`. Rather than depend on a package
  that can't build on the latest GHC, we vendor the handful of definitions
  we actually used (`Rec`/`RecNil`/`RecCons`/`(<:)`) in `JShark.Rec`. This
  can be revisited once `topaz`'s `hashable` bound is relaxed, or if
  building against an older GHC (9.10 and earlier work today with real
  `topaz`).
  Also dropped several unused dependencies
  (`containers`, `free`, `integer-gmp`, `semirings`, `transformers`).
* Added a real test suite (`cabal test`) using `tasty`/`tasty-hunit`.
  When `bun` is on `PATH`, generated JS is `JSON.stringify`'d and checked
  against `evaluate`; otherwise the bun-on-PATH check fails and the rest
  of that group is skipped.
* Completed the reference interpreter (`evaluate`): `Show`, `Eq`, `NEq`,
  `GTh`, `LTh`, `GTEq`, and `LTEq` no longer throw `undefined`.
* Added control flow: a ternary conditional (`If`/`if_`) for pure
  expressions, and effectful `IfE`/`ifE`/`when_` and `While`/`while_` loops.
* Added real combinators for `Option` and `Result`: `some`/`none`/`optionCase`
  and `ok`/`err`/`resultCase`, analogous to `maybe` and `either`.
* Removed the `Element` universe in favor of representing DOM elements as
  `Object DomElement`, unifying them with the rest of the typed object
  machinery; generalized `onClick` to work on any object type.
* Fixed `JShark.Dom.lookupSelector`, which incorrectly called
  `document.getElementById` instead of `document.querySelectorAll`.
* Added `JShark.Dom.createElement`, `setAttribute`, `innerHTML`/`setInnerHTML`,
  and `innerText`/`setInnerText`.
* Fixed a codegen bug where `ForEach`'s loop body statements were emitted
  outside the generated `.forEach` callback instead of inside it.
* Fixed a codegen bug where binding a `Unit`-typed effect (anything besides
  the literal `noOp`) via `Bind` could emit a reference to an undeclared
  variable.
* Added a generic escape hatch to `Expr` for calling into arbitrary JS from
  pure expressions: `ExprFFI`/`exprFfi` (named global functions), `ExprProp`/
  `exprProp` (property access), `ExprMethod`/`exprMethod` and
  `ExprMethodCallback`/`exprMethodCallback` (method calls, including ones
  taking a callback such as `.map`/`.filter`), and `ExprIndex`/`exprIndex`
  (array indexing, the one case of these that the reference interpreter
  can actually run). Also added `UnsafeEffectExpr`/`unsafeEffectExpr` to
  embed an `Effect` (e.g. a `LambdaE` callback, or an object handle) inside
  a pure `Expr`, which unblocks passing callbacks/handles as FFI arguments
  (see the note on `UnsafeEffectExpr` for the soundness caveat).
* Added dedicated `MathUnary`/`MathBinary` primitives (as opposed to going
  through the generic FFI escape hatch) specifically so the reference
  interpreter can compute them using real Haskell math.
* New stdlib modules built on the above primitives:
  - `JShark.Array`: `index`, `length_`, `map_`, `filter_`, `includes`,
    `concat_`, `join`, `push`.
  - `JShark.String`: `length_`, `indexOf`, `slice`, `toUpper`, `toLower`,
    `trim`, `split`, `replace`.
  - `JShark.Json`: `stringify`, `unsafeParse`.
  - `JShark.Math`: extended with `sin`/`cos`/`tan`/`asin`/`acos`/`atan`/
    `sqrt`/`cbrt`/`exp`/`log`/`log2`/`log10`/`floor`/`ceil`/`round`/`trunc`/
    `pow`/`atan2`/`max_`/`min_`/`hypot`/`random`.
  - `JShark.Console`: `log`, `warn`, `error_`, `info`.
  - `JShark.Storage`: `localStorage`/`sessionStorage`, `getItem`, `setItem`,
    `removeItem`, `clear`.
  - `JShark.Timers`: `setTimeout`, `setInterval`, `clearTimeout`,
    `clearInterval`.
  - `JShark.Promise`: `promiseThen`, `promiseCatch` (a minimal `.then`/
    `.catch` wrapper, not a full Promise API).
  - `JShark.Ajax`: added `fetch` and a minimal `FetchResponse` object tag
    (`ok`, `status`); this is a deliberate simplification that hands back
    the resolved response's handle directly rather than fully modeling the
    underlying `Promise<Response>` (chain through `JShark.Promise` if
    needed), and does not model body-streaming/JSON-decoding methods.
* Added `JShark.Api.addEventListener`, a generalized version of `onClick`
  for any DOM event name.
* Added `JShark.Dom.appendChild`, `removeChild`, and `getAttribute`.
* Fixed a bug where `JShark.Dom.lookupId`, `lookupSelector`, and
  `createElement` returned a raw, unbound `Effect`; reusing the same
  returned handle in two different combinator calls would silently
  re-run the underlying FFI call every time (e.g. `createElement` would
  create a new, distinct element on every use of the "same" handle). They
  now bind the result once via `toSyntax`, matching the pattern already
  used by `JShark.Ajax.new`.
* Removed dead code: the unused `Optimization`, `ExprF`, and `Statement`
  types, and an unfinished/unwired `STRef`-based unused-binding-elimination
  pass (`identify`/`unidentify`/`removeUnusedBindings`/`match`/`Together`)
  that relied on `unsafeCoerce`, was never called from anywhere, and
  predates the test suite. A proper constant-folding/dead-binding
  optimization pass remains out of scope for this round of work.
* Fixed a bug in `evaluate`'s `MathUnary "round"` case: it used Haskell's
  `round` (banker's rounding to even, e.g. `round 2.5 == 2`), which
  diverges from JS's `Math.round` (rounds half-way values toward
  +Infinity, e.g. `Math.round(2.5) === 3`, `Math.round(-2.5) === -2`).
  Now computed as `floor(x + 0.5)` to match JS.
* Hardened `evaluate`'s `ExprIndex` case: it previously used `round` (wrong
  rounding direction, per above) and could crash on out-of-bounds access
  with an unhelpful pattern-match failure. Now truncates the index (JS
  coerces array indices via `ToIntegerOrInfinity`, i.e. truncation, not
  rounding) and raises a clear error message on out-of-bounds access.
* Added `UnsafeNullable`/`unsafeNullable`, which reinterprets a value that
  may be a JS `null` (such as the result of an FFI call) as an `Option`;
  sound because `Option` is already represented at the JS level as "null
  for None, the value itself for Some". Used this to fix
  `JShark.Storage.getItem`, which previously claimed to return a `String`
  even though `Storage.getItem` returns `null` when the key is absent;
  it now returns `Option String`, so callers must handle the absent case
  via `optionCase` instead of silently risking a JS `null`.
* De-duplicated `JShark.Console.log`, which reimplemented
  `JShark.Api.consoleLog`; it now just aliases it.

## 0.1.0.0 (YYYY-mm-dd)

* First version. Released on an unsuspecting world.
