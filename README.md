# 🦈 JShark

> **A typed, readable JavaScript EDSL embedded in Haskell.**

[![CI](https://github.com/goolord/jshark/actions/workflows/ci.yml/badge.svg)](https://github.com/goolord/jshark/actions/workflows/ci.yml)
[![GHC 9.14+](https://img.shields.io/badge/GHC-9.14+-8f4e8b?logo=haskell)](https://www.haskell.org/)
[![Cabal 3.12](https://img.shields.io/badge/Cabal-3.12+-5e5086?logo=haskell)](https://www.haskell.org/cabal/)
[![License: BSD-3-Clause](https://img.shields.io/badge/License-BSD_3--Clause-blue.svg)](LICENSE)
[![Live Demos](https://img.shields.io/badge/Live%20Demos-goolord.github.io%2Fjshark-059669?logo=githubpages)](https://goolord.github.io/jshark/)

JShark programs are ordinary Haskell values: the object language is JavaScript, the host is Haskell, and the embeddable subset is typed and modeled on Douglas Crockford's *JavaScript: The Good Parts*.

Binders are higher-order using PHOAS (Parametric Higher-Order Abstract Syntax): a lambda is a Haskell function, so terms cannot reference unbound variables, substitution is function application, and capture is impossible. Statements compose with `do` notation, while literals and operators use standard Haskell typeclasses.

Codegen targets clean, readable JavaScript instead of opaque blobs: variable names and callstack hints are preserved, and pure terms can be evaluated directly in Haskell without a browser.

---

## On "The JavaScript Problem"

The HaskellWiki page on ["The JavaScript Problem"](https://wiki.haskell.org/The_JavaScript_Problem) frames the tension:

1. JavaScript as a language has obvious flaws: weak typing, silent conversions, dynamic scope pitfalls (`this`), and no static guarantees.
2. JavaScript as a platform is inescapable: it is the universal runtime of the browser.

Past Haskell attempts split into distinct camps:
- **Whole-language compilers (GHCJS, Asterius):** Compile full Haskell by shipping GHC's runtime (threads, thunk evaluation, GC) to JS or Wasm. You get full language semantics, but at the cost of multi-megabyte bundles, slow startup, and opaque emitted code.
- **Alternative languages (PureScript, Elm):** Clean functional languages designed for browser runtimes, but separated into distinct ecosystems with their own package managers, compilers, and duplicate type definitions.
- **Untyped AST/quasiquote embeddings (JMacro):** Enforce syntactic validity in Haskell, but defer type safety to runtime.
- **Restricted compilers (Fay):** Compiled a small subset of Haskell without a heavy runtime, but lived outside standard GHC typechecking.

JShark targets the band of applications too complex for a static site, but not heavy enough to justify GHCJS. Instead of running a GHC runtime inside JavaScript, it uses standard GHC to typecheck an embedded language that compiles to clean, dependency-free JS.

---

## Table of Contents

- [On "The JavaScript Problem"](#on-the-javascript-problem)
- [Key Features](#key-features)
- [Live Demos](#live-demos)
- [Quick Start](#quick-start)
  - [Setup and Pragmas](#setup-and-pragmas)
  - [1. Compiling Effects](#1-compiling-effects)
  - [2. Evaluating Pure Expressions in GHCi](#2-evaluating-pure-expressions-in-ghci)
  - [3. Two ASTs: Pure vs. Effectful](#3-two-asts-pure-vs-effectful)
  - [4. JavaScript FFI](#4-javascript-ffi)
- [Monorepo Packages](#monorepo-packages)
- [Design and Limitations](#design-and-limitations)
- [Building and Development](#building-and-development)
  - [Prerequisites](#prerequisites)
  - [Nix](#nix)
  - [Cabal](#cabal)
  - [Tests](#tests)
  - [Dev Server](#dev-server)
  - [Scripts](#scripts)
- [Documentation](#documentation)
- [License](#license)

---

## Key Features

- **Readable output:** Codegen preserves program structure and binder names. A `let`-bound temporary gets its name from the Haskell function that created it (via `HasCallStack`), and lambda arguments keep their Haskell names (`\(a :: Expr f 'Number) (b :: Expr f 'Number) -> a + b` compiles to `(a, b) => a + b`). `readableConfig` emits formatted JS; the default config emits a minified IIFE.
- **Scope safety via PHOAS:** Binders use Parametric Higher-Order Abstract Syntax. Terms cannot refer to out-of-scope variables, and variable capture is impossible.
- **Two typed ASTs:** The API separates pure expressions (`Expr f u`) from side effects (`Effect f u`). Pure code (literals, math, lambdas, arrays, objects, `Option`, `Result`) cannot perform I/O. Effects (DOM updates, console logs, mutation, FFI) form a separate tree joined only at explicit boundaries.
- **One term, two runtimes:** Pure expressions can be evaluated in Haskell via GHCi and tests without a JS engine. Effectful programs run under Bun, optionally with browser globals from `happy-dom`, so DOM code can be tested headlessly.
- **Gradually typed FFI:** `ffi` embeds unchecked JavaScript calls while arguments remain type-checked `Expr` terms via heterogeneous records (`Rec`). Helper functions can monomorphize calls into fully typed wrappers.
- **Haskell as macro system:** Combinators like `map`, `zipWith`, and `groupBy` are ordinary Haskell functions building JS terms. `GHC.Generics` records and sum types map directly to JS objects, and `OverloadedRecordDot` provides field access.

---

## Live Demos

Live examples built from `master`: <https://goolord.github.io/jshark/>

The details pane of each example shows the original Haskell source alongside the generated `Readable` JavaScript.

| Application | Description | Source |
| :--- | :--- | :---: |
| **Breakout** | Canvas 2D game loop, state updates, and collision physics. | [Breakout](examples/src/JShark/Example/Breakout) |
| **TodoMVC** | TodoMVC implementation using `jshark-lucid` for declarative DOM and local storage. | [TodoMvc](examples/src/JShark/Example/TodoMvc) |
| **Synth** | Polyphonic Web Audio synthesizer with `AudioParam` scheduling and typed FFI bindings. | [Synth](examples/src/JShark/Example/Synth) |
| **Life** | Conway's Game of Life with WebGL rendering (PixiJS) and web worker engine. | [Life](examples/src/JShark/Example/Life) |

---

## Quick Start

### Setup and Pragmas

JShark programs require a few standard extensions:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
```

`JShark.Prelude` re-exports the EDSL surface, FFI argument syntax, object constructors, and the compiler. Platform modules (`JShark.Console`, `JShark.Dom`, etc.) should be imported qualified to avoid clashes with `base`:

```haskell
import JShark.Prelude
import qualified JShark.Console as Console
import qualified Data.Text.IO as T
```

### 1. Compiling Effects

A closed effectful program has type `forall f. Effect f 'Unit`. Sequence statements in `EffectSyntax` with `do` notation:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.IO as T
import JShark.Prelude
import qualified JShark.Console as Console

greet :: Expr f 'String -> Effect f 'Unit
greet name = fromSyntax $ do
  Console.log ("hello, " <> name)
  done

main :: IO ()
main = compileEffect readableConfig (greet (string "world")) >>= T.putStrLn
```

Emitted JavaScript:

```javascript
console.log("hello, world");
```

### 2. Evaluating Pure Expressions in GHCi

Pure terms can be evaluated directly in Haskell with no JS engine:

```haskell
ghci> import JShark (evaluateNumber)
ghci> import JShark.Api (number)

ghci> evaluateNumber ((number 10 + number 2) * number 4)
48.0
```

### 3. Two ASTs: Pure vs. Effectful

JShark separates pure computation from side effects:

- `Expr f u`: Pure values (numbers, strings, arrays, objects, functions, `Option`, `Result`).
- `Effect f u`: Statements and mutations (DOM, timers, I/O, FFI).

Four combinators bridge between `Effect` and `EffectSyntax` `do` blocks:

| Combinator | Type | Purpose |
| :--- | :--- | :--- |
| `toSyntax` | `Effect f v -> EffectSyntax f (f v)` | Runs an effect in a `do` block. Re-evaluates each time it is used. |
| `bindExpr` | `Effect f u -> EffectSyntax f (Expr f u)` | Runs an effect and binds its result as an `Expr`. |
| `fromSyntax` | `EffectSyntax f (f v) -> Effect f v` | Packs a `do` block into an `Effect`. |
| `hold` | `Effect f u -> EffectSyntax f (Effect f u)` | Memoizes an effect so subsequent uses reference the same value. |

Blocks end with `done` (`toSyntax noOp`), giving the block type `EffectSyntax f (f 'Unit)`.

### 4. JavaScript FFI

`ffi` embeds arbitrary calls when the core library does not cover an API. Arguments are passed as heterogeneous records (`<:` and `arg`) and stay type-checked `Expr` values:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.IO as T
import JShark.Api
import JShark.Api.Rec (Rec (..), (<:))
import JShark.Compiler

logMax :: Effect f 'Unit
logMax = fromSyntax $ do
  toSyntax_ $ ffi "console.log" (arg "max" <: arg 2 <: arg 9 <: RecNil)
  done

main :: IO ()
main = compileEffect readableConfig logMax >>= T.putStrLn
```

Emitted JavaScript:

```javascript
console.log("max", 2, 9);
```

- **Method calls:** `callMethod el "setAttribute" (arg "class" <: arg "active" <: RecNil)` compiles to `el.setAttribute("class", "active")`.
- **Effectful arguments:** Use `ArgEffect` instead of `arg` when an argument is itself an effect call.
- **Typed wrappers:** While `ffi` takes a string callee name, helper functions can monomorphize calls into type-safe interfaces. See [examples/src/JShark/Example/Synth/Audio.hs](examples/src/JShark/Example/Synth/Audio.hs) for a complete example wrapping Web Audio.

---

## Monorepo Packages

| Package | Directory | Description |
| :--- | :--- | :--- |
| `jshark` | [`packages/jshark`](packages/jshark) | Core EDSL, dual AST, interpreter, and JS compiler. |
| `jshark-lucid` | [`packages/jshark-lucid`](packages/jshark-lucid) | Declarative DOM using Lucid syntax, compiled to `createElement` and event bindings. |
| `jshark-bindgen` | [`packages/jshark-bindgen`](packages/jshark-bindgen) | Generates Haskell `ffi` wrapper modules from TypeScript `.d.ts` or JSDoc. |
| `jshark-hotreload` | [`packages/jshark-hotreload`](packages/jshark-hotreload) | SSE dev-server middleware and file watcher for live reloading. |
| `jshark-examples` | [`examples`](examples) | Four showcase apps, dev server, and static exporter. |
| `jshark-testing` | [`packages/jshark-testing`](packages/jshark-testing) | Shared test/bench support (golden-case helpers, compiler-stage benches). |

### `jshark-lucid`

Compiles Lucid HTML definitions to imperative DOM calls:

```haskell
li_ $ do
  classWhen isDone "completed"
  voidWith_ "input" [type_ "checkbox"] $ on "click" toggle
```

### `jshark-bindgen`

Extracts typed Haskell wrappers from TypeScript declarations:

```bash
cabal run jshark-bindgen -- lib.d.ts --module JShark.Lib
```

Requires `bun` with `@typescript` installed to run the extractor.

---

## Design and Limitations

- **A subset by design:** Idiomatic JS patterns outside the core (classes, `this`, prototype mutation, dynamic property lookup) must go through `ffi`.
- **JavaScript semantics leak through:** Numbers are IEEE 754 doubles (bitwise ops truncate via `ToInt32`), exact integers use `BigInt`, and `Maybe`/`Either` map to JS `Option` (`null`/value) and `Result` (`{ok, value}`).
- **Runtime errors remain possible:** The type system guarantees structural correctness and scope hygiene, but external calls can still fail or throw. Use `catch_` to handle JS exceptions.
- **Not an npm bundler:** External npm libraries must be bundled externally or loaded via script tags, then bound via `ffi` or `jshark-bindgen`.

---

## Building and Development

### Prerequisites

| Tool | Version | Purpose |
| :--- | :--- | :--- |
| **GHC** | 9.14+ | Compiler |
| **Cabal** | 3.12+ | Build tool |
| **LLVM** | 20 (`opt-20`, `llc-20`) | Required by GHC on non-Windows platforms |
| **[Bun](https://bun.sh)** | Current | Running JS-vs-interpreter tests and headless DOM evaluation |

### Nix

`flake.nix` pins the full toolchain (GHC 9.14, Cabal, LLVM 20, Bun, Zig, Biome, Fourmolu, esbuild):

```bash
nix develop
cabal build all
```

### Cabal

```bash
git clone https://github.com/goolord/jshark.git
cd jshark
cabal build all
```

### Tests

Tests compile programs to JavaScript and compare execution against the Haskell interpreter and Bun:

```bash
# Run all test suites
cabal test all --test-show-details=direct

# Run only core compiler tests
cabal test jshark-test --test-show-details=direct

# Run a specific test group
cabal test jshark-test --test-options='-p codegen' --test-show-details=direct
```

See [docs/benchmarking-and-testing.md](docs/benchmarking-and-testing.md) for memory limits, RTS settings, and test filters.

### Dev Server

Starts the local example server with live reloading:

```bash
cabal run exe:jshark-examples
```

Served at `http://localhost:3000`.

Export static HTML/JS for deployment (as used by GitHub Pages):

```bash
cabal run exe:jshark-examples -- export ./dist
```

### Scripts

- `scripts/check-wasm.sh`: Compile-checks Zig kernels.
- `scripts/profile-life.sh`: Headless Chrome profiling for the Life example.
- `scripts/capture-example-screenshots.sh`: Regenerates screenshot assets for documentation.

---

## Documentation

- [JShark Tutorial](docs/tutorial.md): EDSL tour covering syntax, AST primitives, lambdas, records, and Lucid.
- [Benchmarking & Testing Guide](docs/benchmarking-and-testing.md): Test suites, profiling, and RTS configuration.
- [Changelog](CHANGELOG.md): Version history and updates.

---

## License

BSD-3-Clause. See [LICENSE](LICENSE).
