# Revision history for jshark

## Unreleased

* `JShark.Bun.evaluateEffectJSON` compiles a closed `Effect` to an IIFE and
  runs it with `bun`, returning `JSON.stringify` of the result. Pure `Expr`
  still uses the Haskell `evaluate` tree-walk; an `Effect` has FFI,
  mutation, and I/O, so bun is the runtime. The result is JSON text, not a
  `Value`: a `MutableObject` or function has no `Value` constructor. The
  JSON leaves through a temp file, so `Console.log` in the program under
  evaluation does not corrupt it. A run that outlives
  `JShark.Bun.Internal.bunTimeoutMicroseconds` (10s) is killed, since
  `while_` and `Timers` can express a program that never terminates.
* A promise-valued program (`JShark.Promise`, `JShark.Ajax`) is awaited
  before serialization instead of stringifying as `{}`. Only a thenable is
  awaited, so a synchronous program gains no microtask tick that would let
  a pending timer fire first.
* `evaluateEffectJSONWith` takes a `BunConfig`, whose `BunEnv` picks the
  globals the program runs against. `Sandbox` (the default) is bare bun:
  no `document`, no `window`. `HappyDom` registers browser globals
  in-process via `@happy-dom/global-registrator`, so `JShark.Dom`,
  `JShark.Storage`, and `window.location` work; `domBunConfig` is that
  with defaults (`domTimeoutMicroseconds`), and `HappyDomOptions` seeds
  `document.body.innerHTML` and the URL. bun resolves the package with
  `--install=fallback` (`node_modules` first, else its install cache), so
  the first run needs network; the module name is fixed rather than
  configurable, since it lands in an `import` specifier. happy-dom
  implements no 2D canvas, so `Canvas.getContext2d` is `none` there.
  `Bun.WebView` is not used: on Windows with Chrome 151 it fails to attach
  (`'Runtime.evaluate' wasn't found`), and it is still experimental.
* `Array.groupBy` is ES2024 `Object.groupBy` as `[{key, items}]` (first-seen
  keys; not a null-prototype object), row `GroupBy`. `evaluate` and
  `$groupBy` agree.
* `$eq` / `$groupBy` / `$zipWith` are defined once per program instead of
  being re-emitted at every call site.
* Fixed: an array literal dropped `ValueUnit` elements, so `Array.singleton`
  (and `Classes.pure` / `traverse` on `Array`) compiled to `[]`.
* Fixed: `evaluate` of `Array.join` and of `Show` on an array rendered
  `null` / `undefined` as `"null"` / `"undefined"`; JS renders them as `""`.
* `JShark.Classes`: universe-indexed copies of the `base` classes that
  apply (`Functor` through `MonadFix` / `MonadZip` / `Category` /
  `Bifoldable` / `Bitraversable`). `Semigroup` / `Monoid` on
  `Expr f ('Array u)` (and `Option`, `Result`, `Function`) are the
  real `base` classes. `Option` `<>` combines innards like `Maybe`
  (not first-`Some`). `foldr` is `Array.reduceRight`. `Foldable.elem`
  is `.==` / `$eq` on Array, Option, and Result. `Array.zipWith` is
  `$zipWith` (`Math.min` length). `evaluate` of `LetRec` ties the knot
  for any rhs (not only `Lambda`), so `MonadFix (Function r)` works; a
  rhs that forces its own binder now diverges instead of erroring.
* `evaluate` now folds `Array.map` / `filter` / `reduce` / `reduceRight` /
  `groupBy` / `singleton` / `concat` / `includes` / `join` / `length`.
* `evaluate` and JS agree on more Good Parts edges: array index is
  `Math.trunc` + throw (no `a[1.9]` / NaN holes); frozen fields are
  evaluated eagerly and compared by last-wins value; `.==` is `$eq`
  (`===` then structural arrays / plain objects); `Show` of `Result` is
  `String(object)` (`"[object Object]"`).
* Dependency bounds follow PVP for the APIs we import: lower bound is
  the version that added the symbol, upper bound is the next `A`
  (`text < 3`, `lucid < 3`, `scotty < 1`). We do not pin the current `B`.
* One `examples` executable serves Breakout and TodoMVC (`cabal run examples`,
  port 3000). `/` is a directory listing; each app lives at `/<name>`.
  The apps are named libraries (`breakout`, `todo-mvc`); the executable
  only owns the server. Each app page has a details pane of the compiled
  JavaScript, syntax-highlighted. `cabal run examples -- export DIR`
  writes a static site; GitHub Pages publishes it from `master`.
* EDSL helpers: `emptyArray`, `toString`, `loop0`, `whenSomeE`, `assign`;
  `toSyntax` / `toSyntax_` re-exported from `JShark.Api`. Example apps no
  longer mention AST constructors.
* **Breaking:** `addEventListener` and timer callbacks
  (`setTimeout` / `setInterval` / `requestAnimationFrame`) take `Expr`,
  not the raw PHOAS binder `f u`.
* `foreverFrame` in `JShark.Timers` — recurring rAF loop.
* Restored `Result` / `ok` / `err` / `resultCase` / `resultCaseE` — Haskell `Either`, JS `{ok: true, value: v}` / `{ok: false, value: e}`.
* `Throw` / `catch_` (catch binds a `String`); `try_` ignores the binder.
* `LetRec` / `BindRec`, `orElse` / `fromOption`, nested-unary `lambda2`/`apply2`/`lambda3`/`apply3`.
* Statement `if` is `IfE` after `discard` (no `IfS` constructor). `FieldLit` is keyed by `Field r k`.
* `JShark.Generic`: `Generic` records as objects (`toObject` /
  `toObjectArray` / `newRecord`, row `As a`; `ViaGeneric` for `'MutableObject a`).
  Primitives via `ToJS` / `ToValue` (`Int` is IEEE `Number`).
  Sums as `{tag, payload}` (`toSum` / `toSumArray` / `caseSum` / `whenTag` /
  `sumTag`, row `Tagged a`). `Either` stays `Result`.
* Frozen `'Object r` lives on `Expr` (`FrozenLit` / `GetField` / `Object.frozen`).
  Record-dot there is a pure `Expr`. Mutable objects are `'MutableObject`.
* `caseSum` / `on` / `CaseEnd` / `CaseAny` (`Case_`): coverage-checked
  Generic sum case. Named arms are a prefix of `CtorNames a`. Every
  named arm tests its tag; `CaseEnd` throws on leftovers; `CaseAny` is
  a suffix wildcard. `whenTag` is the one-arm matcher.
* Optimizer/codegen inlining applies the PHOAS continuation to an
  `Embed` hole instead of `unsafeCoerce` on binder tags. `evaluateCached`
  has no `Typeable` on the result; `eqT` is only for a `StableName` hit.
* `HasField` on mutable `Effect`/`Expr` so `OverloadedRecordDot`
  (`o.fullName`) is `get`. Frozen `Expr` objects project to `Expr`.
  Binders use `get` or `(Var x).k`.
* `todo-mvc`: `Todo` / `AppState` are `Generic` records (`MutableObjectOf`);
  `render` is `bindRec`, not a persisted field.
* `call0` accepts `ToEffect` (`Expr` or `Effect`).
* `JShark.Canvas`: 2D context (`getContext2d`, rects/path/text,
  `save`/`restore`, transforms). Styles are `Field`s.
* `requestAnimationFrame` in `JShark.Timers`.
* `breakout` executable: Canvas Breakout (`cabal run breakout`, port 3001).
* Discarded `do` blocks keep the last assignment: assign/call refs are
  tagged effectful so a leftover `ifE` ident is still dropped.
* Property access uses `o["0"]` when the key is not a JS identifier.
* `%` / bitwise (`rem_`, `bitAnd`…`ushr`), `parseInt_` (radix required).
* Array `reduce_`, `arraySlice`, `sort_`; `Regex` via `new RegExp`;
  `obj`/`field` literals, `Object.create`, `delete_`, `hasOwn`.
* Qualified stdlib drops the `_` suffix (`Array.length`, `Math.max`,
  `Console.error`, …); those modules `hiding` Prelude/base clashes.
* `IsString` for `Value 'String`, `Expr f 'String`, and `ExprF _ _ 'String`
  so OverloadedStrings literals work at each pure AST layer.
* `Num` / `Fractional` for `Value 'Number` and `Expr f 'Number`
  (`Expr` literals go through `Value`; `ExprF` stays Plus-only).
  `Floating` on `Expr f 'Number` (`sin`, `sqrt`, `**` as `Math.pow`, `pi`).
  Hyperbolics are `Math.sinh` / …, not host identities.
  `JShark.Math` keeps JS-only names (`round`, `atan2`, `max`, `log2`, …).
* `ToEffect` / `ToExpr` lift classes, JS operators (`.==`, `.||`, …), and
  `EffectSyntax` helpers (`hold`, `stmts`, `whenS`, `onClick_`, `getProp`,
  `setProp`, `getProp'`/`setProp'`, `obj`/`objE`, `locationHash`,
  `Array.push_`, …) in `JShark.Api` / `JShark.Array` / `JShark.Dom`.
* Added a `todo-mvc` executable: Scotty serves Lucid HTML with client
  behaviour compiled from JShark (`cabal run todo-mvc`). Client script
  is at `/app.js`; filter is hash-driven; corrupt `localStorage` is ignored.
* `UnsafeObject` is no longer treated as a cheap binding: inlining
  duplicated object literals and broke shared mutable state.
* `LambdaE` / `ForEach` codegen allocate one parameter name and use it
  in both the parameter list and the body (previously they could disagree).
* `onClick` assigns the DOM `onclick` property (not `onClick`).
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
* Codegen runs a constant-folding / dead-binding pass first: arithmetic,
  boolean, and string ops on literals, `if_`/`ifE` of a constant
  condition, `while false`, beta-reduction of `Apply Lambda`, and
  `optionCase`/`resultCase` of known constructors. Cheap literals
  (numbers, strings, bools, unit, nested option/result — not arrays)
  are propagated even under lambdas; unused pure bindings are dropped;
  unused FFI/method/property expressions are kept so their effects
  still run. esbuild may DCE a pure IIFE of a folded literal to empty;
  `JShark.Compiler` retries those as `export default (…)` / ESM and
  strips the export so `compilePure` still returns an expression.
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
* Reintroduced the original `ExprF` fragment and finished its unused-binding
  pass as `JShark.ExprF`: identify installs a unique binder id, a bottom-up
  pass drops dead `LetF`s (so inner DCE can free outer binders), then
  unidentify rebuilds. Binder ids replace the original `STRef` pointer
  equality. `toExprF` converts `Expr (Const Int)` once per binder (no
  deferred re-entry after coerce); `removeUnusedBindingsExpr` runs a
  parallel `Const Int` DCE without `repoly`. Covers only
  Literal/Plus/Let/Lambda/Apply/Var — full-program codegen still uses the
  `Expr`/`Effect` pass above. The empty `Statement` / `Optimization` stubs
  stay deleted.
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
