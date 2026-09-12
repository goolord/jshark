# 🦈 JShark

Write typed JavaScript in Haskell. Generate code you can read.

[![CI](https://github.com/goolord/jshark/actions/workflows/ci.yml/badge.svg)](https://github.com/goolord/jshark/actions/workflows/ci.yml)
[![GHC 9.14+](https://img.shields.io/badge/GHC-9.14+-8f4e8b?logo=haskell)](https://www.haskell.org/)
[![Cabal 3.12](https://img.shields.io/badge/Cabal-3.12+-5e5086?logo=haskell)](https://www.haskell.org/cabal/)
[![License: BSD-3-Clause](https://img.shields.io/badge/License-BSD_3--Clause-blue.svg)](LICENSE)
[![Live Demos](https://img.shields.io/badge/Live%20Demos-goolord.github.io%2Fjshark-059669?logo=githubpages)](https://goolord.github.io/jshark/)

JShark is an embedded domain-specific language (EDSL) for a typed subset of JavaScript, inspired by Douglas Crockford's *JavaScript: The Good Parts*. Programs are ordinary Haskell values, checked by GHC and compiled to JavaScript without a Haskell runtime.

Compose statements with `do` notation, use familiar Haskell operators, and build reusable combinators. Evaluate pure expressions in Haskell or compile them for the browser.

## Table of Contents

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

## Key Features

- **Readable output.** `readableConfig` formats JavaScript with meaningful binder names and call-stack hints. The default emits a compact, immediately invoked function (IIFE).
- **Safe scope.** Parametric higher-order abstract syntax (PHOAS) uses Haskell functions for binders, preventing unbound variables and variable capture in typed terms.
- **Explicit effects.** `Expr f u` describes pure computation; `Effect f u` describes mutation, DOM updates, and foreign calls.
- **Headless testing.** Evaluate pure terms in Haskell. Run generated JavaScript in Bun, with `happy-dom` for browser APIs.
- **JavaScript interop.** Call external APIs through `ffi`, write typed wrappers, or generate bindings from TypeScript declarations.
- **Haskell abstractions.** Build JavaScript with ordinary Haskell functions. Convert `Generic` records and sums to objects, and access fields with `OverloadedRecordDot`.

---

## Live Demos

Live examples built from `master`: <https://goolord.github.io/jshark/>

Each demo pairs its Haskell source with the generated JavaScript.

| Application | Description | Source |
| :--- | :--- | :---: |
| **Breakout** | Canvas 2D game loop, state updates, and collision physics. | [Breakout](examples/src/JShark/Example/Breakout) |
| **TodoMVC** | TodoMVC implementation using `jshark-lucid` for declarative DOM and local storage. | [TodoMvc](examples/src/JShark/Example/TodoMvc) |
| **Synth** | Polyphonic Web Audio synthesizer with `AudioParam` scheduling and typed FFI bindings. | [Synth](examples/src/JShark/Example/Synth) |
| **Life** | Conway's Game of Life with WebGL rendering (PixiJS) and web worker engine. | [Life](examples/src/JShark/Example/Life) |

---

## Quick Start

### Setup and Pragmas

The examples below use these extensions:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
```

`JShark.Prelude` exports the core API and compiler. Import platform modules qualified to avoid name clashes:

```haskell
import JShark.Prelude
import qualified JShark.Console as Console
import qualified Data.ByteString.Char8 as BS
```

### 1. Compiling Effects

A closed effectful program has type `forall f. Effect f 'Unit`. Sequence statements in `EffectSyntax` with `do` notation:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as BS
import JShark.Prelude
import qualified JShark.Console as Console

greet :: Expr f 'String -> Effect f 'Unit
greet name = fromSyntax $ do
  Console.log ("hello, " <> name)
  done

main :: IO ()
main = compileEffect readableConfig (greet (string "world")) >>= BS.putStrLn
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

Use `EffectSyntax` for `do` blocks, with these combinators to bind results and build an `Effect`:

| Combinator | Type | Purpose |
| :--- | :--- | :--- |
| `toSyntax` | `Effect f v -> EffectSyntax f (f v)` | Binds an effect's result as a PHOAS variable. |
| `bindExpr` | `Effect f u -> EffectSyntax f (Expr f u)` | Runs an effect and binds its result as an `Expr`. |
| `fromSyntax` | `EffectSyntax f (f v) -> Effect f v` | Packs a `do` block into an `Effect`. |
| `hold` | `Effect f u -> EffectSyntax f (Effect f u)` | Binds an effect's result as a reusable `Effect` handle. |

Blocks end with `done` (`toSyntax noOp`), giving the block type `EffectSyntax f (f 'Unit)`.

### 4. JavaScript FFI

Use `ffi` for APIs outside the core library. Build argument lists with `arg` and `<:`. Arguments have JShark types, but the foreign function's signature is unchecked:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as BS
import JShark.Api
import JShark.Api.Rec (Rec (..), (<:))
import JShark.Compiler

logMax :: Effect f 'Unit
logMax = fromSyntax $ do
  toSyntax_ $ ffi "console.log" (arg "max" <: arg 2 <: arg 9 <: RecNil)
  done

main :: IO ()
main = compileEffect readableConfig logMax >>= BS.putStrLn
```

Emitted JavaScript:

```javascript
console.log("max", 2, 9);
```

- **Method calls:** `callMethod el "setAttribute" (arg "class" <: arg "active" <: RecNil)` compiles to `el.setAttribute("class", "active")`.
- **Effectful arguments:** Use `ArgEffect` instead of `arg` when an argument is itself an effect call.
- **Typed wrappers:** Give foreign calls concrete Haskell signatures so callers use a checked interface. The wrapper author is responsible for matching the JavaScript API. See the [Web Audio bindings](examples/src/JShark/Example/Synth/Audio.hs).

---

## Monorepo Packages

| Package | Directory | Description |
| :--- | :--- | :--- |
| `jshark` | [`packages/jshark`](packages/jshark) | Core EDSL, dual AST, interpreter, and JS compiler. |
| `jshark-lucid` | [`packages/jshark-lucid`](packages/jshark-lucid) | Declarative DOM using Lucid syntax, compiled to `createElement` and event bindings. |
| `jshark-bindgen` | [`packages/jshark-bindgen`](packages/jshark-bindgen) | Generates Haskell `ffi` wrapper modules from TypeScript `.d.ts` or JSDoc. |
| `jshark-hotreload` | [`packages/jshark-hotreload`](packages/jshark-hotreload) | SSE dev-server middleware and file watcher for live reloading. |
| `jshark-examples` | [`examples`](examples) | Four showcase apps, dev server, and static exporter. |

The internal `jshark:testing` sublibrary provides shared test and benchmark support.

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

Requires Bun and the `typescript` npm package. See the [bindgen README](packages/jshark-bindgen/README.md) for supported types.

---

## Design and Limitations

- **A subset by design:** Idiomatic JS patterns outside the core (classes, `this`, prototype mutation, dynamic property lookup) must go through `ffi`.
- **JavaScript semantics:** Numbers are IEEE 754 doubles; bitwise operations use 32-bit conversions. Use `BigInt` for exact integers. `Option` and `Result` use tagged objects: `{some, value}` and `{ok, value}`.
- **Runtime errors:** Types check the embedded program, not external APIs. Foreign calls can fail or throw; use `catch_` to handle JavaScript exceptions.
- **Not an npm bundler:** External npm libraries must be bundled externally or loaded via script tags, then bound via `ffi` or `jshark-bindgen`.

---

## Building and Development

### Prerequisites

| Tool | Version | Purpose |
| :--- | :--- | :--- |
| **GHC** | 9.14+ | Compiler |
| **Cabal** | 3.12+ | Build tool |
| **LLVM** | 20 (`opt-20`, `llc-20`) | Used by this repository's non-Windows build configuration |
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

Tests cover the interpreter, compiler, bindings, and examples. Runtime checks compare generated JavaScript in Bun with the Haskell interpreter:

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

Run the examples with live reloading at `http://localhost:3000`:

```bash
cabal run exe:jshark-examples
```

Export static HTML and JavaScript for deployment:

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
- [Contributing](CONTRIBUTING.md): Local build/test/format workflow and commit conventions.
- [Changelog](CHANGELOG.md): Version history and updates.

---

## License

BSD-3-Clause. See [LICENSE](LICENSE).
