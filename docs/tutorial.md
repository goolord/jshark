# JShark tutorial

A tour of the EDSL for Haskell programmers who want to write browser
JavaScript. Everything here compiles; the full programs live in
`examples/src/JShark/Example/*` and the test suites, which compile each
snippet and check the emitted JavaScript against both the Haskell
interpreter and `bun`.

## Setup

A JShark program is ordinary Haskell. The canonical pragma block:

```haskell
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
```

(Or set them once as `default-extensions` in your `.cabal` file. The
examples in this repo repeat the block per module, which is why every
example file starts the same way.)

Import `JShark.Prelude` (the EDSL surface, FFI argument syntax, object
literals, and the compiler in one module) plus the platform modules you
need, qualified:

```haskell
import JShark.Prelude
import qualified JShark.Console as Console
import qualified JShark.Dom as Dom
```

Platform modules share many names with base (`JShark.String.length`,
`JShark.Map.lookup`), so keep them qualified. `JShark.Api.Classes`
provides `Functor`/`Monad`-style instances for the EDSL types and is
also designed for qualified import.

Compiling is pure string production — no JS runtime involved:

```
ghci> T.putStrLn =<< compileEffectSyntax readableConfig (Console.log ("hi" :: Expr f 'String))
console.log("hi");
```

`readableConfig` emits the pretty snippet you see above;
`defaultCompilerConfig` emits a minified IIFE (via esbuild/Closure/Terser
when one is on `PATH`). `compileEffectSyntax` takes the do-notation form
directly; `compileEffect` takes a closed `Effect`.

## Two trees, one bridge

The API is split in two:

- `Expr f u` — pure expressions: literals, operators, functions, arrays,
  objects, `Option`/`Result`. Evaluable in Haskell without a browser
  (`evaluate`, `evaluateNumber`, `evaluateBigInt`).
- `Effect f u` — statements: mutation, DOM, FFI, control flow.

Do-notation works on `EffectSyntax`, and the bridge is four words:

| word        | direction                       | type                              |
|-------------|---------------------------------|-----------------------------------|
| `toSyntax`  | `Effect` → do-block value       | `Effect f v -> EffectSyntax f (f v)` |
| `bindExpr`  | same, reified as an `Expr`      | `Effect f u -> EffectSyntax f (Expr f u)` |
| `fromSyntax`| whole do-block → `Effect`       | `EffectSyntax f (f v) -> Effect f v` |
| `hold`      | keep an `Effect` for reuse      | `Effect f u -> EffectSyntax f (Effect f u)` |

`bindExpr` gives you an `Expr` you can pass to pure functions;
`toSyntax`/`hold` give you the runtime handle form. Reusing the result
of `toSyntax` re-executes the effect; reusing a `hold`ed binding
references the same value. Statements end with `done`
(`toSyntax noOp`) so the block has type `EffectSyntax f (f 'Unit)`.

```haskell
greet :: Expr f 'String -> Effect f 'Unit
greet name = fromSyntax $ do
  Console.log ("hello, " <> name)
  done
```

Literals and operators use the standard Haskell classes: `number 1 + number 2`,
`("a" :: Expr f 'String) <> "b"`, and `OverloadedRecordDot` for object
fields (below). Comparisons are dotted (`.==`, `.!=`, `.&&`) because the
bare spellings are taken by the Haskell classes; JS semantics leak
through on purpose (`Number` is an IEEE double, `rem_`/`quot_` truncate,
bitwise ops go through `ToInt32`, exact integers are `BigInt`).

## Functions and control flow

```haskell
add :: Expr f 'Number -> Expr f 'Number -> Expr f ('Function 'Number 'Number)
add a b = lambda (\x -> x + a + b)
```

`lambda` is pure; `lambdaE` may bind and sequence effects. Lets are
`let_ x (\v -> ...)` — single-use lets inline, multi-use lets stay
`const` in the output, and under `readableConfig` the emitted name is
the Haskell function that created it (recovered via `HasCallStack`).
Branching: `if_` is the ternary on pure values, `ifE`/`whenS`/`ifS`
compose statements, `while_` loops, `forRange_` is the counting loop,
`forEach` iterates arrays.

Options and results are tagged JS values: `some x` / `none` compile to
plain values/`null`, `ok a` / `err e` to `{ok, value}`. Branch with
`optionCase` (expressions), `optionCaseE` + `whenSomeS`/`whenNoneS`
(statements), `resultCase` / `resultCaseE`.

## DOM and typed events

`JShark.Dom` wraps element lookup and mutation. Event handlers receive a
typed `Event` — no annotations, no `getProp'`:

```haskell
wire :: Effect f ('MutableObject Dom.DomElement) -> EffectSyntax f (f 'Unit)
wire el = do
  board <- Dom.byId "board"
  addEventListenerS "keydown" board $ \e -> do
    k <- eventKey e
    toSyntax_ (callMethod el "flash" (arg k <: RecNil))
    done
```

`addEventListenerS` takes the handler directly in `EffectSyntax` (the
`Effect`-returning variant is `addEventListener`). Typed accessors cover
`eventKey`, `eventCode`, `eventRepeat`, `eventPointerId`, `eventButton`,
`eventShiftKey`, `eventClientX/Y`, `eventOffsetX/Y`, and
`Dom.eventTarget`. For anything else, `getProp' e "name"` stays
available (unchecked).

## Records, sums, and objects

`Generic` Haskell records become JS objects and Haskell sums become
tagged objects:

```haskell
data Person = Person { fullName :: Text, years :: Double }
  deriving Generic

js <- compileEffect readableConfig (fromSyntax (G.toObject (Person "Ada" 36)))
-- {fullName: "Ada", years: 36}
```

Read fields back with `Object.get @"fullName" o` or plain `o.fullName`
(`OverloadedRecordDot` on both `Effect` and `Expr` handles). Literal
objects without `Generic` use `Object.obj [Object.field @"x" (number 1)]`
(frozen counterparts: `Object.frozen`). `G.toSum`/`G.caseSum`/`G.on`
cover tagged sums with a `Tag`-based match; `G.whenTag @"Red"` is the
two-branch shortcut.

Functions with named parameters use `namedLambda`/`namedLambdaRow` and
`JShark.Api.Params` rows (`Param "x" 'Number`); binder names land in the
generated JavaScript, which keeps the readable output honest.

## FFI

When the typed core does not cover a call, `ffi` embeds it:

```haskell
logMax :: Effect f 'Unit
logMax = fromSyntax $ do
  toSyntax_ $ ffi "console.log" (arg "max" <: arg 2 <: arg 9 <: RecNil)
  done
-- console.log("max", 2, 9);
```

- The callee is free text, emitted verbatim — a typo is a runtime error,
  not a compile error. The tests parse-check emitted output.
- Arguments are `arg` (an `Expr`), `argEffect` (an effectful callback,
  rendered inline), string/number literals via `OverloadedStrings`/`num`.
- `callMethod receiver "method" args` puts an object handle in front.
- `ffi` classifies its callee string: unparenthesized `=>` arrows become
  function values, IIFEs stay calls. `ffiExpr` always emits a bare
  expression (for `typeof`, comparisons, property reads).
- To keep type safety at the boundary, wrap `ffi` in a monomorphic
  helper — see `examples/src/JShark/Example/Synth/Audio.hs` for real
  wrappers, or generate whole modules from TypeScript with
  `jshark-bindgen`.

## Testing without a browser

Pure terms evaluate in Haskell — the test suite cross-checks every
snippet above with `evaluate`/`evaluateNumber`:

```
ghci> evaluateNumber ((number 1 + number 2) * number 4)
12.0
```

Effectful programs run under `bun`, optionally with browser globals from
happy-dom, so DOM code is testable headlessly (see
`examples/test/BunTests.hs`).

## Where to go next

- `examples/src/JShark/Example/` — five real apps: TodoMVC (with
  `jshark-lucid` templates), a Canvas Breakout, a Web Audio synth, a
  WebGL Game of Life, and the HVM2/WASM Mandelbrot lab.
- `packages/jshark-lucid` — describe DOM in Lucid syntax, compile to
  `createElement` calls.
- `packages/jshark-bindgen` — generate typed `ffi` wrappers from
  TypeScript declarations.
- `wasm/hvm2/` — the Bend/HVM2 kernel pipeline for numeric hot spots.
- `docs/benchmarking-and-testing.md` — profiling and benchmarking the
  compiler itself.
