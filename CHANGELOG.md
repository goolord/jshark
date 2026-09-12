# Revision history for jshark

## Unreleased

* Compiler correctness (each backed by a regression test that fails on the
  previous behavior):

  * `EffectSyntax.(*>)` no longer drops the continuation of
    `EffectSyntaxUnpure`; a sequenced do-block statement keeps every effect
    it runs.
  * `lowerFnBodyTags` threads a fresh binder stamp instead of restarting at
    `-2`, so a `Fn` body no longer aliases its own parameter or an
    enclosing `let`.
  * Pure `if_` and `optionCase`, and `&&`/`||`, keep branch-local
    declarations inside their branch instead of hoisting both sides and
    evaluating the untaken one.
  * `optConstantFoldNumOnce` copies its input with `thaw` and freezes every
    written column; it no longer mutates the caller's `FlatSoA` (and no
    longer relies on that mutation for the `B` column).
  * `elimIrBind` only moves a single-use binding to its use site when the
    bound term is pure or an alias. Splicing an impure effect past other
    effects reordered evaluation in minified output
    (`foo(); bar()` compiled to `bar(); foo()`).

* Negative zero literals now compile to `-0.0` instead of `0`.

* Testing: `JShark.Bun.Internal` gains `runJSTagged`/`runProgramTagged`,
  which serialize through a tagger that keeps `undefined` vs `null`, `NaN`,
  the infinities, `-0`, and `BigInt` distinct; added gated `bun` tests for
  each.

* Benchmarks: the standalone `LifeEmit`/`LifeFullEmit`/`LifePhases`
  timers force their result before stopping the clock (they previously
  timed only thunk allocation).

## 0.1.0.0 (2026-09-11)

* First version. Released on an unsuspecting world.

* Release polish: shared test/bench support moves to the public
  `jshark:testing` sublibrary (replacing the `jshark-testing` package);
  every library package gains Hackage metadata (`description`,
  `tested-with`, `source-repository`, version bounds, per-package
  README/CHANGELOG, `extra-doc-files`); `-Werror` moves behind a
  `werror` cabal flag (still enabled for local builds); the dev-only
  per-phase profiling executables and `profile*`/`CompileTiming`
  helpers are removed; the public API is documented and the exposed
  compiler internals are labelled internal/unstable.
* Internal dead code and copy-paste are gone, ~3.3k LOC: the compiler drops
  unused entries (`irExprFromClosed`, `irOptimized{Effect,Expr}FromClosed`,
  `nestedDummy`, `renderFFIForm`, the unstyled `prepareFlat*Program`,
  `fromOption`, `apply3`, `setProp'`, `ToExpr`, `ParamAt`, `sumTag`,
  `mergeModules`), the 11 numeric smart constructors share one `numBinE`
  combinator, the math lookup/match tables merged per arity, `Ir` exports
  `IrNode (..)` instead of a 77-line constructor list, `childMeta` folds
  over `irNodeChildren`, `elimIrLet`/`elimIrBind` share `elimBinder`,
  `Hoist`/`Hoist.Canonical` fold into `JsShim`/`Codegen.Core`,
  `flatNodeIsEffect` is an opcode range check, and the eight copy-pasted
  emit-plan blocks share helpers. Emitted JS is byte-identical.

* The compile-diagnostics cluster collapses to a minimal live progress bar.
  `JShark.Compiler.CompileReport` and `JShark.Compiler.CompileTerminal` are
  deleted; `CompileProgress` keeps the per-job board, phase sub-bars, and
  done lines, and drops the per-job stats/timing machinery (the only
  consumer discarded the stats and the stats table). `compileJobsLabeled`
  returns `[Text]`; the unused `compileEffects`/`compilePures` (+`Labeled`)
  batch entries and the `configProgressSlot` field are gone. `--progress`
  output is unchanged apart from the removed stats table.

* jshark-bindgen is TypeScript-extractor-only: the hand-written fallback
  parser (`ParseDts`, `ParseJs`, ~1,050 LOC, `--no-ts`) and the unused
  `--json` mode / `encodeModule` are deleted; the `Ty` folds share one
  `tyAndChildren` traversal. CI already required bun.

* jshark-hotreload drops the unused EDSL lifecycle hooks
  (`onDispose`/`hotState*`) and no longer depends on `jshark`; the
  unreachable `drainToLBS` branch is gone. jshark-lucid's
  `JShark.Lucid.HotReload` keeps only `hotReloadClient`. The drifted
  vendored copy `examples/static/js/jshark-reload.js` is deleted (pages
  load `/__jshark/client.js`).

* Shared test/bench support lives in the `jshark:testing` sublibrary
  (`Test.Support`, `CaptureStderr`, `Bench.Stages`), consumed by the core
  test/bench and the example package; the four-example registry
  is `JShark.Example.Registry` (was hand-written three times), and the
  bun-gating scaffold is shared (`BunGate`). Core tests gain golden-case
  combinators (`effectCodeCase`/`pureCodeCase`/`effectContains`/
  `evalBoolCase`) used by 183 of 309 cases; test names and literals are
  unchanged.


* HVM2/Bend support is removed. The `Hvm2Kernel` expression constructor,
  `Hvm2KernelEntry`, `JShark.Api.hvm2Kernel`, `JShark.Api.loadHvm2Wasm`,
  and the `IrHvm2Ref` / `FE_Hvm2Ref` / `FE_HVM2REF` flat-IR nodes are gone;
  the `JShark.Hvm2`, `JShark.Compiler.EmitBend`, and
  `JShark.Compiler.Hvm2Lint` modules are deleted. `CompilerConfig` drops
  `configWarnHvm2Candidates` and the `--warn-hvm2-candidates` flag, and the
  compile-stats table drops the `lint` phase column. The HVM2 Lab
  (Mandelbrot) example, its `build-hvm2-demo-wasm` tool, the `wasm/hvm2`
  Zig pipeline, and its static worker assets are deleted. The remaining
  examples are Breakout, TodoMVC, Synth, and Life.

* Dead compiler internals are gone: `JShark.Compiler.Optimize.Hvm2`
  (`collectHvm2Kernels`, no callers), the unused `cgTag` codegen counter
  and `allocTag`, the write-only `FlatEmitPlan` `fepReach` mask, and the
  `renderJSCompact` alias of `renderJS`. The `closedEffectNodes` /
  `closedExprNodes` aliases collapse into `optimizedEffectSize` /
  `optimizedExprSize`. Emitted JS is byte-identical.

* Core no longer owns external minification or the on-disk minify cache.
  `CompilerBackend`/`ClosureLevel`/`CompilerClosureConfig`/
  `CompilerEsbuildConfig`/`CompilerTerserConfig`/`CacheStrategy`, the
  `compileWith`/`compileWithPure`/`tryCompileWith` post-processors, and the
  named `compileClosure`/`compileEsbuild`/`compileTerser` helpers are removed.
  `CompilerConfig` drops `configBackend`/`configCache`/`configFallback`.
  Codegen still emits compact IIFEs; run esbuild/terser/Closure over the
  output yourself. Emitted JS is byte-identical.

* The flat IR collapses from three modules into one:
  `JShark.Compiler.Flat` now owns packing, the frozen SoA view, the bulk
  passes, and decode. The 78 hand-written opcode constants and the
  78-row `flatOpTable` (`FlatEnc`) become a `data FlatOp` deriving
  `Enum`/`Bounded` (one constructor per `FlatNode`), stored as `Word16`
  via `opCode`/`flatOpOf`. `decodeOp` becomes a `case`, so GHC proves
  decode coverage at compile time — a new opcode without a decode arm is
  a build error, not a runtime one. The unused subtree-size machinery
  (`flatSoaSubtreeSizes`, `attachFlatSoaSubtreeSizes`,
  `computeFlatSoaSubtreeSizes`, `flatSoaIdentBudget`) is deleted; it ran
  on every pack and had no callers. Breaking: `JShark.Compiler.FlatEnc`
  and `JShark.Compiler.FlatSoA` no longer exist; import
  `JShark.Compiler.Flat`. Emitted JS is byte-identical.

* The Mandelbrot demo's WASM payload moves out of the core library:
  `emitKernelWasmBridge` (the SIMD128 fast path, HVM2 net-reduction
  driver, and 8-ary `jshark_grid` bridge), the demo Bend module assembly
  (`ParTree` prelude, `jshark_grid`, 4096-leaf `main`), and the
  bend→C→zig orchestration (`compileHvm2GenC`/`compileHvm2Wasm`,
  `Hvm2Config`) now live in `JShark.Example.Hvm2Demo.WasmBuild`
  (examples package). Core `JShark.Hvm2` keeps the generic Bend emitter
  surface (`bendKernel`, `bendDefNames`, `bendDefExports`,
  `emitKernelExportsC`, `sanitizeKernelCForWasm`). `bendModule` is
  renamed `demoBendModule` and `bendModuleFromTree` (unused) is deleted.

* Sixteen compiler-internal modules (`Lower`, `Evaluate`, `Emit`,
  `Hoist`, `Codegen.Flat`, `Codegen.Stmt`, `JsShim`, `JsNum`, `JsFormat`,
  `Binder`, `CompileReport`, `CompileTerminal`, `Optimize.Hvm2`,
  `Process`, `Hoist.Canonical`, `Api.Prim`) move from the library's
  exposed-modules to other-modules: they compile the same but no longer
  sit on the public Haddock surface. The `JShark` facade and
  `JShark.Compiler` re-export the public entry points.

* Dead code removed: `compileJS` (Compiler), `mapFixedArgs`/`foldFixed`
  and the `sortByM` alias (Evaluate), `mergePreamble`/`assertDisjoint`
  (JsShim), the identity wrapper `emitFlatSiblings` (Codegen.Flat), the
  duplicate `peelLambdasFn` (EmitBend), and the PHOAS-optimizer timing
  plumbing left behind by its deletion (`PhoasPrepareTiming`,
  `recordJobPhoasPrepare`, the always-zero `phopt` stats column).

* Internal dedup: Hvm2Lint's private `irKids`/`isEffectNode` become the
  shared `Ir.irNodeChildren` + `Flat.irNodeIsEffect` (one child layout to
  maintain per constructor); Codegen.Flat's `flatPureChild`/`flatEffectChild`
  twins merge into `flatChild`, and the apply/lambda-spine emitters are
  parameterized over expression vs effect position; the compiler batch
  driver drops two adapter layers; `padLeft`/`padRight`, the digit-check
  helpers, and the last-wins field dedup each collapse to one definition;
  the test suite builds `CompilerConfig` via `defaultCompilerConfig{...}`
  record updates instead of ten positional 8-field blocks.

* New ergonomics surface (breaking, see the new `JShark.Prelude` for a
  one-import quick start):
  - `JShark.Prelude` re-exports `JShark.Api`, `JShark.Api.Rec`
    (`Rec`/`<:`), the object-literal constructors, and `JShark.Compiler`;
    a typical program drops from 4-8 imports to 1-2.
  - The `addEventListener` callback now receives a typed
    `Event` (`JShark.Api.Event`) instead of an untyped
    `Expr f ('MutableObject ())`, and `addEventListenerS` takes the
    handler directly in `EffectSyntax` (no `stmts` wrap, no inline
    annotations). Typed field accessors `eventKey`, `eventCode`,
    `eventRepeat`, `eventPointerId`, `eventButton`, `eventShiftKey`,
    `eventClientX/Y`, `eventOffsetX/Y` (and `Dom.eventTarget` replacing
    the old `getProp' e "..."` dances in the examples.
  - New combinators filling gaps the examples kept re-implementing:
    `toNumber` (JS `Number()` coercion), `whenNoneS` (the `whenSomeS`
    complement), `argEffect` (smart constructor next to `arg`),
    `Dom.byId` (literal-id element lookup), and `compileEffectSyntax`
    (absorbs the `fromSyntax` wrap at the compile boundary).
  - `JShark.Api`'s module header documents the `_`/`S`/prime naming
    conventions.

* Documentation: Haddock for the hot `JShark.Api` names (literals,
  `lambda`/`lambdaE`, `let_`, control flow, `Option`/`Result`, FFI) and
  the previously bare platform modules (`String`, `Promise`, `Ajax`,
  `Math`); new `docs/tutorial.md` covering the two-tree model, the
  `EffectSyntax` bridge, typed DOM events, `Generic` records/sums,
  `Params` rows, `ffi` classification, and the headless test story;
  README now points at `scripts/profile-life.sh` and
  `scripts/capture-example-screenshots.sh`.

* `CaptureStderr` (both copies) hoists its Windows `pipe` arity
  difference out of the do-block; fourmolu can now parse and format the
  whole repo (`scripts/format.sh` exits clean).

* The IR GADT pair `IrExpr`/`IrEffect` merges into one untyped
  `data IrNode` (`JShark.Compiler.Ir`). The type indices did no checking
  post-lowering — the EDSL construction already type-checked the program —
  so they carried only runtime data the flat IR already reifies. One
  constructor per `FlatNode` opcode; kernel/method/fixed/fn-literal payloads
  flatten onto `IrNode`, and object fields become `IrField` (name + child;
  the `KnownSymbol`/`Typeable` per-field dictionaries reduce to `Text`
  names). The optimizer is a single `optIr`/`metaIr`/`occursIr`/
  `lazyOccursIr`; the `IrEmbedEff` bridge node and the sole `unsafeCoerce`
  (`replaceIrVarExpr`) are gone. `Lower.hs`, `Flat.hs` (single `runPack`),
  `FlatSoA.hs` (single `packProgramDirect`), `EmitBend.hs` (no
  `SomeIrExpr`), `Hvm2Lint.hs` (single `IrNode` scan), and
  `Codegen/Core.hs` (single `flatPrepareFromIr`) are updated to match.
  Breaking: `IrExpr`, `IrEffect`, `optIrExpr`, `optIrEffect`,
  `metaIrExpr`, `metaIrEffect`, and the `IrKernel`/`IrMethod`/`IrFnBody`/
  `IrArg`/`IrFieldLit`/`IrFixedArgs` wrapper types no longer exist. The
  `JShark.*` facade names are unchanged.

  Emitted output is byte-identical to before: the merged optimizer keeps
  two redundant single-use effect binds (`v <- e; pure v`) under `keepLets`
  that the twin optimizer inlined, and `buildFlatEmitPlan` reserved a
  phantom identifier for each (shifting later generated names by +2 in the
  readable `life` output). The plan now skips binder names for binds that
  codegen flattens into their RHS (`flatBindEffect`), restoring the
  pre-merge numbering; readable and minified goldens are byte-identical.

* Flat-opt purity computed once, at pack. `fsaPure` is filled by
  `FlatSoA.computeFlatSoaPure` (one backward sweep in pack order) when
  the SoA is frozen, instead of being zeroed at pack and re-derived by
  the `propagatePureFlagsPar` fixpoint in `optimizeFlatPack` — purity was
  computed twice (IR `IrMeta` at opt, SoA propagation after) with the IR
  result discarded. The numeric constant fold stays in the flat pass: it
  is a post-inline wave, not a duplicate (`elimIrLet` on the tree can
  create `lit op lit` nodes after the kernel was visited; the single
  bottom-up IR pass never revisits). `propagatePureFlagsPass` /
  `propagatePureFlagsPar` / `propagatePureWithStats` are deleted, and
  `FlatOptProfile` drops `fopPureSec` / `fopPurePasses`.
  `JShark.Compiler.Optimize` (a thin re-export shim since the PHOAS
  optimizer was deleted) is gone; its entry points moved to
  `JShark.Compiler.Lower` (`collectHvm2Kernels` now comes from
  `JShark.Compiler.Optimize.Hvm2` directly). No public `JShark.*` names
  changed.

* One compilation pipeline. Pure expressions and effectful programs now
  share a single path — lower to the first-order IR (`JShark.Compiler.Ir`),
  one IR optimizer (all constant/structural folds live in `optIrExpr` /
  `optIrEffect`), pack to the flat SoA, one emitter (`Codegen.Flat`).
  The PHOAS optimizer (`JShark.Compiler.Optimize` and its
  `Analysis`/`Elim`/`Fold`/`Metadata` satellites) and the direct-PHOAS
  emitter (`JShark.Compiler.Codegen.Phoas`) are deleted; its shared
  statement helpers moved to `JShark.Compiler.Codegen.Stmt`.
  `JShark.Compiler.FlatView` (a pure `FlatSoA` alias) is gone.

  * `Stamp` is a plain `Int` binder tag again. `Embed`/`EmbedEff` PHOAS
    inlining holes and the `Lower.reify*` inverse are removed; so is the
    `JShark.Compiler.Flatten` module. `IrEmbedEff` remains as the IR node
    for an inlined effect used in expression position.
  * `IrLet` carries the source hint so readable mode keeps pure-let names.
  * A single canonical opcode table (`FlatEnc.flatOpTable`) is round-tripped
    by `FlatTest` (decode totality, tag bijection, pinned operand columns).
  * Pure programs report through the flat timing hooks; the obsolete
    `optIrLargeThreshold` routing is gone.
  * Real programs compile faster (Life e2e ~9% on readable) and emit less
    (folds); the adversarial 800-deep `longChain` IR-opt bench is ~25%
    slower from per-node fold/hint work.

  Breaking API: `pureAST`/`pureASTWith`/`pureProgram` output text changed
  (one emitter), and these are gone: `optimize`, `optimizeWith`,
  `optimizeEffect`, `optimizeEffectIr`, `optimizeEffectFromIr`,
  `phoasNodeCountFromIr`, `optIrLargeThreshold`, `nodeCountExpr`,
  `nodeCountEff`, `effectfulASTIr`, `effectfulASTFromFlat`,
  `preparePureProgram*`. `effectfulASTFromSoA` stays. `Stamp` is a
  single-constructor `Int` tag.

* `jshark-bindgen` executable: generate JShark FFI wrappers from TypeScript
  `.d.ts` / `.ts` (and JS with JSDoc). `cabal run jshark-bindgen -- FILE`.
  Not part of the `jshark` library.

* Readable JS output is formatted with [Biome](https://biomejs.dev/) when
  `biome` is on PATH (`nix develop` and CI install it). `prettyJS` is now
  `Text -> IO Text` (was pure). On Biome failure, compiles log to stderr and
  keep compact emit.

* `'BigInt`. Host `Integer` via `ToJS` / `ToValue` / `bigInt`.
  Literals emit `Nn` (negatives parenthesized). Kernel ops share
  `NumericU` with `'Number` (`rem_`, bitwise, shifts); `quot_` is
  BigInt-only truncating `/`. `toBigInt` / `fromBigInt` /
  `parseBigInt_` (sign and `0x`/`0b`/`0o`). Number `Num` instances
  are `INCOHERENT` so `seqN + 1` still defaults to `'Number`.

* Life grid/canvas helpers (`seedSoupRegion`, `paintGridCells`, `u8CopyRegion`,
  `u8FillRegion`, `forRange2`, …) moved from `JShark.Api` to `GridApi` in the
  Life example library (breaking change for external callers of those names).

* `fillRgbaImageData` replaces `clearRgbaImageData` (parameterized RGBA fill for
  `ImageData.data` buffers). `u8FillRegion` clears a rectangular slice of a
  row-major `Uint8Array` via `forRange` + `u8Set`.

* `'Uint8Array`. Host `ByteArray` (base) via `ToJS` / `ToValue` /
  `uint8Array`. Literals emit `new Uint8Array([…])`. A runtime-sized
  buffer is `newByteArray n` (`ffi` → `new Uint8Array(n)`), not a
  `'MutableObject` row. Allocation has identity; JS can write the object.
  No freeze API. Tagged-sum payloads are `ObjectLit` with an untyped
  extra `payload` key on `FieldLit` (`Tagged` only types `tag`).
  `FieldLitEffect` carries effectful object fields in `ObjectLit` (replaces
  `UnsafeEffectExpr` in `Generic`). Optimizer bind inlining uses `EmbedEff`
  on `Stamp` (not surface API). Removed byte-array `Effect`
  constructors. `'Fn` / `fnLit` / `toFn` with `JShark.Params` rows for
  n-ary positional callbacks (replaces `jsUncurry` / `'JsFn2` /
  `Uncurry2`); `Array.sort` uses `toFn`. `lambdaRow` / `toLambda` for
  n-ary curried `'Function` nests via the same rows (replaces `lambda2` /
  `lambda3`). `Array.toSorted` — pure copy via ES2023
  `Array.prototype.toSorted` (`Std ToSorted`).
  Fixed-arity stdlib ops (`Math.*`, `Array.length`, `String.*`, …) share
  one `Std (Fixed op args)` constructor with a `FixedOp` GADT and
  `JShark.Prim` for host math, JS names, and codegen templates (replaces
  `Math1` / `Math2` / `Un` / `Bin` / `Tern`). Higher-order stdlib
  (`Map`, `Filter`, `Reduce`, …) stays on separate `Std` constructors.
  `expr1` / `expr2` / `expr3` wrap `Fixed` as `Expr` for call sites.
  HO array stdlib (`Map`, `Filter`, `Reduce`, `ToSorted`, `From`) unified
  under `Std (Method …)` with shared `evalMethod` / `optMethod` /
  `renderMethod` dispatch.
  Good Parts kernel operators (`+`, `===`, `&&`, `typeof`, …) unified under
  `Std (Kernel …)` with shared `evalKernel` / `optKernel` / `renderKernel`
  dispatch; pattern synonyms (`Plus`, `Eq`, …) preserve the public
  surface and `Num` instances (compare ops via `mkGTh`/`mkLTh`/… helpers).
  `FFIForm`
  tags lambda vs call (not string matching).
  `Expr` is a Good Parts kernel plus one `Std` constructor for pure JS
  stdlib (`Math.*`, `Array.prototype.map` / `filter` / `reduce`, …).
  `zipWith` and `groupBy` are Haskell functions over that tree
  (`Array.from` / `reduce`+`filter`). `Array.index` is `Math.trunc`, a
  bounds check, kernel `a[i]`, and `Error` (throw). Codegen hoists
  `$valueEq` for kernel `Eq` (`===`, then `$arrayEq`, `$deepEqual`,
  `$uint8ArrayEq`; not general JS equality). `base >= 4.17`.


* Fixed: codegen and the optimizer numbered PHOAS tags from the same
  counter (both `-2` downward), so `countEffect` could attribute a
  binder's uses to a leftover optimizer tag, count zero, and drop the
  `const` while every use of it rendered empty — emitting statements like
  `.setAttribute("class", "view")` with no receiver, or `.type = ;`. Any
  `when_` followed by a binding could trigger it, as could a binding
  inside nested branches, and the result was JavaScript that did not
  parse. The optimizer now walks the even negatives and codegen the odd
  ones, so the two numberings can never name the same binder.
* New `synth` example: a polyphonic Web Audio synthesizer at `/synth`
  (`cabal run examples`). `Audio` binds `AudioContext`, oscillators, a
  biquad filter, a compressor, and an analyser through `ffi`, so the rest
  of the example stays in typed JShark — `new` is reachable only because
  `ffi` is free text, and phantom handles (`AudioCtx`, `Node`, `Param`)
  keep `connect` from accepting a param. Pitch and the amplitude envelope
  are `AudioParam` automation, so timing lives on the audio thread rather
  than in JavaScript; the only per-frame work is the meter. Voices are
  keyed by note, pointers by `pointerId`, and `blur` / `pointercancel`
  release held notes.
* The test suite parse-checks every example's emitted JavaScript with bun
  (`ExampleTests`). Compiling the Haskell says nothing about whether the
  emitted program is syntactically valid; the program is bound inside an
  arrow that is never called, so a syntax error fails while no DOM or
  audio call runs.

* `JShark.Bun.evaluateEffectJSON` compiles a closed `Effect` to an IIFE and
  runs it with `bun`, returning `JSON.stringify` of the result. Pure `Expr`
  still uses the Haskell `evaluate` tree-walk; an `Effect` has FFI,
  mutation, and I/O, so bun is the runtime. The result is JSON text, not a
  `Value`: a `MutableObject` or function has no `Value` constructor. The
  JSON leaves through a temp file, so `Console.log` in the program under
  evaluation does not corrupt it. A run that outlives
  `JShark.Bun.Internal.bunTimeoutMicroseconds` (10s) is killed, since
  `while_` and `Timers` can express a program that never terminates.
* New `jshark-lucid` library. `JShark.Lucid` describes a DOM fragment with
  Lucid's own combinators and compiles it to `createElement` /
  `setAttribute` / `appendChild` calls. Lucid's `Html` is a function to a
  `Builder`, not a tree, so a finished value cannot be walked; what gets
  reused is the *syntax* — `Term` and `With` are open classes and
  `Attribute` is a pair of `Text`, so container elements (`li_`, `div_`, …)
  and every attribute (`class_`, `href_`, …) work unchanged at `JsHtml`.
  Void elements (`input_`, `br_`) are fixed to Lucid's `HtmlT`, so `void_`
  covers those. Dynamic parts sit in the child block and apply to the
  enclosing element: `dynText`, `dynAttr`, `classWhen`, `prop`, `on`.
  `classWhen` is `classList.toggle(c, test)`, so the element always carries
  a `class` attribute, empty when nothing matched. `todo-mvc` builds its
  `<li>` rows with it.
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
