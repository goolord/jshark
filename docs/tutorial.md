# JShark tutorial

A practical tour for Haskell programmers writing browser JavaScript.
The snippets introduce the API; the [examples](../examples/src/JShark/Example)
show complete applications. Tests cover compilation, host evaluation,
and JavaScript execution in Bun.

## Setup

A JShark program is ordinary Haskell. These extensions cover the examples
in this guide:

```haskell
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
```

You can also set these as `default-extensions` in your `.cabal` file.

Import `JShark.Prelude` for the core API and compiler, then add qualified
imports for the platform modules you need:

```haskell
import JShark.Prelude
import qualified Data.ByteString.Char8 as BS
import qualified JShark.Console as Console
import qualified JShark.Dom as Dom
```

Platform modules share many names with base (`JShark.String.length`,
`JShark.Map.lookup`), so keep them qualified. `JShark.Api.Classes`
provides `Functor`/`Monad`-style instances for the EDSL types and is
also designed for qualified import.

Compilation returns JavaScript as bytes and needs no JavaScript runtime:

```
ghci> BS.putStrLn =<< compileEffectSyntax readableConfig (Console.log ("hi" :: Expr f 'String))
console.log("hi");
```

`readableConfig` emits a formatted snippet;
`defaultCompilerConfig` emits a compact IIFE (minify it with an external
tool if you want). `compileEffectSyntax` takes the do-notation form
directly; `compileEffect` takes a closed `Effect`.

## Expressions and effects

The API is split in two:

- `Expr f u` — pure expressions: literals, operators, functions, arrays,
  objects, `Option`/`Result`. Evaluable in Haskell without a browser
  (`evaluate`, `evaluateNumber`, `evaluateBigInt`).
- `Effect f u` — statements: mutation, DOM, FFI, control flow.

Write `do` blocks in `EffectSyntax`. Four combinators connect them to `Effect`:

| Combinator  | Purpose                         | Type                              |
|-------------|---------------------------------|-----------------------------------|
| `toSyntax`  | `Effect` → do-block value       | `Effect f v -> EffectSyntax f (f v)` |
| `bindExpr`  | same, reified as an `Expr`      | `Effect f u -> EffectSyntax f (Expr f u)` |
| `fromSyntax`| whole do-block → `Effect`       | `EffectSyntax f (f v) -> Effect f v` |
| `hold`      | keep an `Effect` for reuse      | `Effect f u -> EffectSyntax f (Effect f u)` |

`toSyntax` binds a result as a PHOAS variable. `bindExpr` exposes that
result as an `Expr` for pure functions; `hold` exposes it as a reusable
`Effect` handle. Each refers to the bound value rather than repeating the
original effect. End a unit-returning block with `done` (`toSyntax noOp`)
to give it type `EffectSyntax f (f 'Unit)`.

```haskell
greet :: Expr f 'String -> Effect f 'Unit
greet name = fromSyntax $ do
  Console.log ("hello, " <> name)
  done
```

Literals and operators use the standard Haskell classes: `number 1 + number 2`,
`("a" :: Expr f 'String) <> "b"`, and `OverloadedRecordDot` for object
fields (below). Comparisons are dotted (`.==`, `.!=`, `.&&`) because the
bare spellings belong to Haskell classes. Operations follow JavaScript
semantics: `Number` is an IEEE 754 double, `quot_` truncates, `rem_` gives
the remainder, and bitwise operators use 32-bit conversions. Use `BigInt`
for exact integers.

## Functions and control flow

```haskell
add :: Expr f 'Number -> Expr f 'Number -> Expr f ('Function 'Number 'Number)
add a b = lambda (\x -> x + a + b)
```

`lambda` is pure; `lambdaE` may bind and sequence effects. Bind pure values
with `let_ x (\v -> ...)`: single-use bindings inline, reused bindings stay
`const` in the output, and under `readableConfig` the emitted name is
the Haskell function that created it (recovered via `HasCallStack`).
Branching: `if_` is the ternary on pure values, `ifE`/`whenS`/`ifS`
compose statements, `while_` loops, `forRange_` is the counting loop,
`forEach` iterates arrays.

Options are tagged JS values: `none` is `{some: false}`, `some x` is
`{some: true, value: x}` (so `Option (Option a)` faithfully nests).
At a foreign boundary `unsafeNullable` converts a native `null`/value
from an FFI result into a tagged option, and `unsafeOptionToNative`
unwraps a tagged option back to native `null`/value when passing it to a
foreign parameter declared `T | null`; generated bindings use both.
`Result` is `{ok, value}`. Branch with `optionCase` (expressions),
`optionCaseE` + `whenSomeS`/`whenNoneS` (statements), `resultCase` /
`resultCaseE`.

Host evaluation mirrors the compiled semantics for pure terms, but keeps
failures distinct: `evaluate` returns a `Value` (and throws), while
`tryEvaluate` reports `EvalJsFailure` for a JS-like throw (including a
checked index out of bounds) and `EvalUnsupported` for constructs the
interpreter does not model (functions, effectful object fields, ops with
no host rule). Structural equality (`structuralEq`/`.==`) compares
arrays, objects, options, and results by value; reference `===` is not
exposed. JS numeric edge cases are preserved (`NaN`, `±Infinity`, `-0`,
`Math.round` halves toward `+Infinity`).

## DOM and typed events

`JShark.Dom` wraps element lookup and mutation. Event handlers receive a
typed `Event` with accessors for common fields:

```haskell
wire :: Effect f ('MutableObject Dom.DomElement) -> EffectSyntax f (f 'Unit)
wire el = do
  board <- Dom.byId "board"
  addEventListenerS "keydown" board $ \e -> do
    k <- eventKey e
    toSyntax_ (callMethod el "flash" (arg k <: RecNil))
    done
  done
```

`addEventListenerS` takes the handler directly in `EffectSyntax` (the
`Effect`-returning variant is `addEventListener`). Typed accessors cover
`eventKey`, `eventCode`, `eventRepeat`, `eventPointerId`, `eventButton`,
`eventShiftKey`, `eventClientX/Y`, `eventOffsetX/Y`, and
`Dom.eventTarget`. Use the unchecked `getProp' e "name"` for other fields.

## Records, sums, and objects

`Generic` Haskell records become JS objects and Haskell sums become
tagged objects:

```haskell
data Person = Person { fullName :: Text, years :: Double }
  deriving Generic

js <- compileEffect readableConfig (G.toObject (Person "Ada" 36))
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
generated JavaScript.

## FFI

When the typed core does not cover a call, `ffi` embeds it:

```haskell
logMax :: Effect f 'Unit
logMax = fromSyntax $ do
  toSyntax_ $ ffi "console.log" (arg "max" <: arg 2 <: arg 9 <: RecNil)
  done
-- console.log("max", 2, 9);
```

- The callee is unchecked JavaScript text. GHC cannot catch misspelled
  names or mismatched foreign signatures.
- Arguments are `arg` (an `Expr`), `argEffect` (an effectful callback,
  rendered inline), and string/number literals via `OverloadedStrings`/`number`.
- `callMethod receiver "method" args` puts an object handle in front.
- `ffi` classifies its callee string: unparenthesized `=>` arrows become
  function values, IIFEs stay calls. `ffiExpr` always emits a bare
  expression (for `typeof`, comparisons, property reads).
- Give wrappers concrete type signatures that match the foreign API.
  See the [Web Audio bindings](../examples/src/JShark/Example/Synth/Audio.hs),
  or generate modules from TypeScript with `jshark-bindgen`.

## Testing without a browser

Evaluate pure terms in Haskell with `evaluate` or a typed helper:

```
ghci> evaluateNumber ((number 1 + number 2) * number 4)
12.0
```

Run effectful programs in Bun, with `happy-dom` when they need browser
globals. See [BunTests.hs](../examples/test/BunTests.hs) for headless tests.

## Where to go next

- [Examples](../examples/src/JShark/Example) — TodoMVC, Breakout, a Web Audio
  synth, and a WebGL Game of Life.
- [jshark-lucid](../packages/jshark-lucid) — DOM templates in Lucid syntax.
- [jshark-bindgen](../packages/jshark-bindgen) — typed wrappers from TypeScript.
- [Testing and benchmarking](benchmarking-and-testing.md) — test filters,
  compiler benchmarks, and profiling.
