# 🦈 JShark

> **A typed, readable JavaScript EDSL embedded in Haskell.**

[![CI](https://github.com/goolord/jshark/actions/workflows/ci.yml/badge.svg)](https://github.com/goolord/jshark/actions/workflows/ci.yml)
[![GHC 9.14+](https://img.shields.io/badge/GHC-9.14+-8f4e8b?logo=haskell)](https://www.haskell.org/)
[![Cabal 3.12](https://img.shields.io/badge/Cabal-3.12+-5e5086?logo=haskell)](https://www.haskell.org/cabal/)
[![License: BSD-3-Clause](https://img.shields.io/badge/License-BSD_3--Clause-blue.svg)](LICENSE)
[![Live Demos](https://img.shields.io/badge/Live%20Demos-goolord.github.io%2Fjshark-059669?logo=githubpages)](https://goolord.github.io/jshark/)

**JShark** allows you to write client-side web applications in idiomatic Haskell that compile into clean, dependency-free, human-readable JavaScript. JShark programs are ordinary Haskell values: the object language is JavaScript, the host is Haskell, and the embeddable subset is strongly typed and modeled on Douglas Crockford's *JavaScript: The Good Parts*.

---

## On "The JavaScript Problem"

For over a decade, the Haskell community has wrestled with what the HaskellWiki famously codified as [**"The JavaScript Problem"**](https://wiki.haskell.org/The_JavaScript_Problem):

> 1. **JavaScript, the language**, is rife with well-known hazards: weak typing, silent coercions, late binding, unpredictable `this` scoping, and a total lack of compile-time guarantees.
> 2. **JavaScript, the platform**, is inescapable: it is the ubiquitous, non-negotiable runtime of the web browser.

Historically, functional programmers have attempted to navigate this tension from several distinct angles:

- **The Heavyweight Emulators ([GHCJS](https://github.com/ghcjs/ghcjs), [Asterius](https://github.com/tweag/asterius)):**  
  These compile whole Haskell programs by shipping a full simulated GHC runtime into JavaScript or WebAssembly—complete with a green-thread scheduler, laziness thunk evaluation, and garbage collection. While this preserves standard Haskell semantics, it comes at a steep price: multi-megabyte bundle sizes, sluggish cold starts, impedance mismatches with DOM APIs, and emitted output that is an impenetrable blob of machine code.

- **The Separate Frontend Languages ([PureScript](https://www.purescript.org/), [Elm](https://elm-lang.org/)):**  
  Rather than hauling GHC into the browser, these invent entirely new functional languages designed around browser execution models. While they generate clean JavaScript, they force you into a segregated ecosystem: distinct compilers, separate build tools, different package managers, and the chore of manually keeping frontend and backend data types in sync.

- **The Untyped Macro Embeddings ([JMacro](https://wiki.haskell.org/JMacro)):**  
  Quasiquoted templating systems that offer syntactic correctness checks in Haskell, but defer type safety to runtime or external linters.

- **The Lightweight Subset Compilers ([Fay](https://github.com/faylang/fay/wiki)):**  
  Compilers for a constrained subset of Haskell without the full GHC runtime, which pioneered the idea of lean emitted code but operated outside GHC's native typechecker.

### JShark's Stance: Host the Types, Not the Runtime

**JShark occupies the sweet spot:** it is designed for the broad band of web applications that are more complex than a static site, but not complex enough to justify dragging an entire GHC runtime into the client.

Rather than teaching the JavaScript virtual machine how to evaluate lazy thunks and schedule lightweight threads, JShark flips the equation: **use Haskell's type system to discipline JavaScript.**

1. **Standard GHC, Zero Forking:**  
   JShark is an Embedded Domain-Specific Language (EDSL). JShark programs are plain Haskell values compiled with your regular GHC toolchain. Your backend and frontend can live in the same codebase, sharing records, combinators, and build infrastructure.

2. **Zero Runtime Overhead:**  
   There is no GHC runtime layer in the browser. JShark terms compile directly to slim, native JavaScript: Haskell functions become JS arrow functions, arrays become JS arrays, and `let` bindings become JS variables.

3. **Mathematically Hygienic via PHOAS:**  
   By modeling binders with **Parametric Higher-Order Abstract Syntax (PHOAS)**, a lambda in JShark is a native Haskell function. Unbound variables and variable capture bugs are impossible by construction.

4. **Auditable, Human-Readable JavaScript:**  
   Instead of generating an unreadable compiler artifact, JShark produces readable code that preserves your Haskell binder names and recovers temporary variable names from the callstack via `HasCallStack`. You can step through it cleanly in Chrome DevTools or inspect it in code review.

In short, JShark solves "The JavaScript Problem" by giving you Haskell's compile-time peace of mind without saddling your users with a multi-megabyte runtime penalty.

---

## Table of Contents

- [On "The JavaScript Problem"](#on-the-javascript-problem)
- [Key Features](#key-features)
- [Live Demos & Showcase](#live-demos--showcase)
- [Quick Start](#quick-start)
  - [Pragmas and Setup](#pragmas-and-setup)
  - [1. Compiling an Effectful Program](#1-compiling-an-effectful-program)
  - [2. Evaluating Pure Expressions in GHCi](#2-evaluating-pure-expressions-in-ghci)
  - [3. The Core Mental Model (Two Typed ASTs)](#3-the-core-mental-model-two-typed-asts)
  - [4. Interoperating with JavaScript (FFI)](#4-interoperating-with-javascript-ffi)
- [Monorepo Packages](#monorepo-packages)
- [Design Philosophy & Limitations](#design-philosophy--limitations)
- [Building & Development](#building--development)
  - [Prerequisites](#prerequisites)
  - [Using Nix](#using-nix)
  - [Using Cabal](#using-cabal)
  - [Running the Test Suites](#running-the-test-suites)
  - [Running the Dev Server](#running-the-dev-server)
  - [Helper Scripts](#helper-scripts)
- [Documentation & Resources](#documentation--resources)
- [License](#license)

---

## Key Features

- **JavaScript You Can Actually Read**  
  Codegen preserves your program's structure and variable names instead of emitting an unreadable blob. Binders keep their original Haskell names, and `let`-bound temporaries infer readable names from the calling function via `HasCallStack`. For debugging, `readableConfig` emits pretty, formatted JavaScript; for production, `defaultCompilerConfig` emits a minified IIFE.

- **Guaranteed Scope Safety with PHOAS**  
  Binders use **Parametric Higher-Order Abstract Syntax (PHOAS)**. A lambda is a native Haskell function: terms can never reference an unbound variable, variable capture is mathematically impossible, and substitution is plain function application.

- **Two Typed ASTs (Pure vs. Effectful)**  
  The API strictly separates pure computation (`Expr f u`) from side effects (`Effect f u`). Pure code (literals, math, lambdas, arrays, objects, `Option`, `Result`) forms a strict subset you can safely reason about. Effects (DOM updates, console logs, mutable state, timers, FFI) form a separate tree joined only at explicit, typed seams.

- **One Term, Dual Runtimes**  
  Pure expressions can be evaluated directly in Haskell with no JS engine required—enabling blazingly fast unit tests. Effectful programs can be executed headlessly under [Bun](https://bun.sh) (with optional browser globals via `happy-dom`), allowing DOM and storage code to be tested without launching a full browser.

- **Gradual, Type-Checked FFI**  
  When an API is not in the standard library, `ffi` lets you call arbitrary JavaScript while keeping arguments type-checked `Expr`s via heterogeneous records (`Rec`). Polymorphic calls can be easily locked down into type-safe wrappers using standard Haskell signatures.

- **Haskell as the Metaprogramming Engine**  
  Because JShark is an EDSL, libraries are just Haskell code. Combinators like `map`, `zipWith`, and `groupBy` are ordinary Haskell functions generating JavaScript ASTs. `GHC.Generics` records and sum types map automatically to JS objects, and `OverloadedRecordDot` allows natural record field access.

---

## Live Demos & Showcase

Explore live demonstrations built directly from the `master` branch:

**[Launch Live Demo Site](https://goolord.github.io/jshark/)**

> [!TIP]
> On the demo site, open the **Details pane** to inspect the original Haskell source code side-by-side with the generated `Readable` JavaScript output.

| Application | Description | Source |
| :--- | :--- | :---: |
| **Breakout** | Classic 2D arcade game running on HTML5 Canvas with a 60 FPS animation loop, state updates, and collision physics. | [Breakout](examples/src/JShark/Example/Breakout) |
| **TodoMVC** | Complete TodoMVC implementation using `jshark-lucid` for reactive, declarative HTML templating and local storage persistence. | [TodoMvc](examples/src/JShark/Example/TodoMvc) |
| **Web Audio Synthesizer** | Polyphonic synthesizer with custom UI. Web Audio nodes and `AudioParam` sample-accurate scheduling are controlled via typed FFI wrappers. | [Synth](examples/src/JShark/Example/Synth) |
| **Game of Life** | Conway's Game of Life rendered with WebGL (PixiJS) backed by a high-performance web worker computation engine. | [Life](examples/src/JShark/Example/Life) |
| **HVM2 & WASM Lab** | Interactive Mandelbrot zoom lab benchmarking three computation backends: plain JavaScript, SIMD WebAssembly (Zig), and HVM2 interaction net reduction. | [Hvm2Demo](examples/src/JShark/Example/Hvm2Demo) |

---

## Quick Start

### Pragmas and Setup

A JShark program is standard Haskell. Typical modules use the following language extensions:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
```

Import `JShark.Prelude` to bring the core EDSL, FFI records, and compiler into scope, along with any platform modules you need (qualified to prevent name clashes with `base`):

```haskell
import JShark.Prelude
import qualified JShark.Console as Console
import qualified Data.Text.IO as T
```

---

### 1. Compiling an Effectful Program

A closed program has type `forall f. Effect f 'Unit`. You sequence statements inside `EffectSyntax` using standard `do` notation:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.IO as T
import JShark.Prelude
import qualified JShark.Console as Console

-- | An effectful function taking a typed JS string expression
greet :: Expr f 'String -> Effect f 'Unit
greet name = fromSyntax $ do
  Console.log ("hello, " <> name)
  done

main :: IO ()
main = do
  js <- compileEffect readableConfig (greet (string "world"))
  T.putStrLn js
```

When compiled, JShark generates clean JavaScript:

```javascript
console.log("hello, world");
```

---

### 2. Evaluating Pure Expressions in GHCi

Pure expressions can be evaluated directly in Haskell without invoking a JavaScript runtime:

```haskell
ghci> import JShark (evaluateNumber)
ghci> import JShark.Api (number)

ghci> evaluateNumber ((number 10 + number 2) * number 4)
48.0
```

---

### 3. The Core Mental Model (Two Typed ASTs)

JShark enforces a strict boundary between pure computations and stateful operations:

- **`Expr f u`**: Represents pure expressions (numbers, strings, arrays, objects, functions, `Option`, `Result`). Completely free of side effects.
- **`Effect f u`**: Represents statements and actions (DOM mutation, timers, I/O, FFI calls).

To bridge between pure expressions and effectful blocks in `do` notation, JShark provides four fundamental combinators:

| Combinator | Direction | Type Signature | Purpose |
| :--- | :--- | :--- | :--- |
| `toSyntax` | `Effect` → `do` block | `Effect f v -> EffectSyntax f (f v)` | Executes an effect in a `do` block. Re-evaluates each time it is referenced. |
| `bindExpr` | `Effect` → `Expr` | `Effect f u -> EffectSyntax f (Expr f u)` | Evaluates an effect and reifies the result as an immutable `Expr` value. |
| `fromSyntax` | `do` block → `Effect` | `EffectSyntax f (f v) -> Effect f v` | Closes a `do` block into an `Effect` value. |
| `hold` | `Effect` → `Effect` handle | `Effect f u -> EffectSyntax f (Effect f u)` | Memoizes an effectful computation so multiple uses share the same result. |

Every statement sequence ends with `done` (an alias for `toSyntax noOp`), ensuring the block finishes with type `EffectSyntax f (f 'Unit)`.

---

### 4. Interoperating with JavaScript (FFI)

When the standard library doesn't cover a browser API, use `ffi` or `callMethod`. Arguments are passed as heterogeneous lists using `<:` and `arg`, remaining strictly type-checked `Expr` values:

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

Generated JavaScript:

```javascript
console.log("max", 2, 9);
```

#### Method Calls and Effectful Arguments
- **Method calls:** `callMethod el "setAttribute" (arg "class" <: arg "active" <: RecNil)` compiles to `el.setAttribute("class", "active")`.
- **Effectful arguments:** To pass an argument that is itself the result of an effect, use the `ArgEffect` constructor instead of `arg`.
- **Static typing:** While `ffi` accepts dynamic function names, you can enforce static type safety across your project by writing typed Haskell helper functions (see [examples/src/JShark/Example/Synth/Audio.hs](examples/src/JShark/Example/Synth/Audio.hs) for a complete example binding the Web Audio API).

---

## Monorepo Packages

This repository is organized as a Cabal multi-package project:

| Package | Directory | Description |
| :--- | :--- | :--- |
| **`jshark`** | [`packages/jshark`](packages/jshark) | The core EDSL, dual AST definitions, Haskell interpreter, and JavaScript code generator. |
| **`jshark-lucid`** | [`packages/jshark-lucid`](packages/jshark-lucid) | Declarative HTML and DOM builder using Lucid syntax that compiles directly to DOM manipulation calls. |
| **`jshark-bindgen`** | [`packages/jshark-bindgen`](packages/jshark-bindgen) | CLI utility to automatically generate typed Haskell FFI modules from TypeScript `.d.ts` declaration files or JSDoc comments. |
| **`jshark-hotreload`** | [`packages/jshark-hotreload`](packages/jshark-hotreload) | Development server infrastructure providing SSE (Server-Sent Events) live reload, WAI middleware, and filesystem watching. |
| **`jshark-examples`** | [`examples`](examples) | The five showcase applications, example runner, dev server, and static export tool. |

### `jshark-lucid` Example

Describe DOM trees using familiar Lucid syntax:

```haskell
li_ $ do
  classWhen isDone "completed"
  voidWith_ "input" [type_ "checkbox"] $ on "click" toggle
```

### `jshark-bindgen` Usage

Generate typed Haskell FFI modules directly from TypeScript types:

```bash
cabal run jshark-bindgen -- lib.d.ts --module JShark.Lib
```

*(Full TypeScript parsing leverages `bun` and `@typescript`; pass `--no-ts` to use the built-in fallback parser).*

---

## Design Philosophy & Limitations

JShark deliberately makes specific pragmatic design choices:

- **A Typed Subset by Design:**  
  Advanced or idiomatic JS patterns outside the core (such as ES6 `class` hierarchies, `this` rebinding, prototype manipulation, and dynamic property indexing) are intentionally excluded. They must be accessed via `ffi`.
- **JavaScript Semantics Intentionally Surface:**  
  The target runtime is JavaScript:
  - `Number` is an IEEE 754 double precision float (bitwise operations truncate to `ToInt32`).
  - Arbitrary precision integers use the distinct `BigInt` type.
  - `Maybe` and `Either` correspond to JavaScript `Option` (`null` / value) and `Result` (`{ok, value}`).
- **Runtime Error Boundaries:**  
  Haskell's type safety guarantees structural consistency and prevents variable capture, but cannot prevent the browser environment from throwing exceptions (e.g., failed network calls or invalid DOM states). Use `catch_` to handle runtime exceptions.
- **Not an npm Bundler:**  
  JShark does not resolve npm package module graphs directly. Third-party libraries are integrated via script tags or bundlers, with wrappers created using `jshark-bindgen` or `ffi`.

---

## Building & Development

### Prerequisites

| Tool | Minimum Version | Required For |
| :--- | :--- | :--- |
| **GHC** | 9.14+ | Compiling the Haskell codebase |
| **Cabal** | 3.12+ | Building packages and managing dependencies |
| **LLVM** | 20 (`opt-20`, `llc-20`) | GHC code generation on non-Windows platforms |
| **[Bun](https://bun.sh)** | Latest | Running JS-vs-interpreter test suites & headless DOM tests |

---

### Using Nix

If you use [Nix](https://nixos.org/), the included `flake.nix` provides a fully pinned development environment including GHC 9.14, Cabal, LLVM 20, Bun, Zig, and formatting tools:

```bash
# Enter the development shell
nix develop

# Build all packages
cabal build all
```

---

### Using Cabal

Clone the repository and build with Cabal:

```bash
git clone https://github.com/goolord/jshark.git
cd jshark

# Build all packages
cabal build all
```

---

### Running the Test Suites

The test suite validates the compiler by compiling programs to JavaScript and verifying execution against both the internal Haskell interpreter and Bun:

```bash
# Run all test suites
cabal test all --test-show-details=direct

# Run only the core compiler tests
cabal test jshark-test --test-show-details=direct

# Run a specific test group (e.g. codegen)
cabal test jshark-test --test-options='-p codegen' --test-show-details=direct
```

> [!NOTE]
> For details on memory caps, RTS options, and test filters, see the [Benchmarking & Testing Guide](docs/benchmarking-and-testing.md).

---

### Running the Dev Server

Launch the interactive showcase server locally (includes automatic hot-reloading):

```bash
cabal run exe:jshark-examples
```

Once running, navigate to `http://localhost:3000` to browse all five applications and inspect their generated JavaScript.

To export a static copy of the demo site (for GitHub Pages deployment):

```bash
cabal run exe:jshark-examples -- export ./output-dir
```

---

### Helper Scripts

Located in the `scripts/` directory:

- `scripts/check-wasm.sh`: Rebuilds the vendored HVM2 WebAssembly binaries and compile-checks Zig kernels.
- `scripts/profile-life.sh`: Drives headless Chrome profiling runs on the Life WebGL example.
- `scripts/capture-example-screenshots.sh`: Regenerates the application screenshots used by the documentation.

---

## Documentation & Resources

- **[JShark Tutorial](docs/tutorial.md):** In-depth walkthrough covering syntax, AST primitives, lambdas, objects, and Lucid integration.
- **[Benchmarking & Testing Guide](docs/benchmarking-and-testing.md):** Detailed guide to RTS profiling flags, test suites, and performance benches.
- **[Changelog](CHANGELOG.md):** Release history, breaking changes, and migration guides.

---

## License

This project is licensed under the **BSD-3-Clause License**. See the [LICENSE](LICENSE) file for details.
