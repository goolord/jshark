<div align="center">

# 🦈 JShark

**A typed JavaScript EDSL for Haskell, with a compiler that emits readable JavaScript.**

[![CI](https://github.com/goolord/jshark/actions/workflows/ci.yml/badge.svg)](https://github.com/goolord/jshark/actions/workflows/ci.yml)
[![Live Demos](https://img.shields.io/badge/demos-goolord.github.io%2Fjshark-059669?logo=githubpages)](https://goolord.github.io/jshark/)
[![GHC 9.14+](https://img.shields.io/badge/GHC-9.14+-8f4e8b?logo=haskell)](https://www.haskell.org/)
[![License: BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE)

</div>

---

JShark embeds a typed subset of JavaScript in Haskell, inspired by
Crockford's *JavaScript: The Good Parts*. A program is a Haskell
value built from two GADTs indexed by a `Universe` kind: `Expr f u` for pure
expressions and `Effect f u` for effectful statements. GHC typechecks the program, and the
jshark compiler lowers it to JavaScript with no runtime library. Standard library functions
that make the language more sane (structural equality, `groupBy`) are printed in a preamble
only when a program uses them.

```haskell
greet :: Expr f 'String -> Effect f 'Unit
greet name = fromSyntax $ do
  Console.log ("hello, " <> name)
  done

main :: IO ()
main = compileEffect readableConfig (greet (string "world")) >>= BS.putStrLn
```

```javascript
console.log("hello, world");
```

`greet` is applied to a literal here, so the optimizer folds the
concatenation before emitting.

## Design

### Types

`Universe` models JavaScript's runtime types: `Number` (an IEEE 754 double
with 32-bit bitwise operators), `BigInt`, `String`, `Bool`, `Unit`,
`Array u`, `Function u v` (unary, nested for currying), `Fn us v` (an
uncurried n-ary JS function), `Option u`, `Result e a`, `Map k v`, `Set u`,
`Uint8Array`, and frozen or mutable objects over a Haskell row type.
`Option` compiles to `{some, value}` and `Result` to `{ok, value}`, so nested
options stay distinct. `Generic` records become plain objects and `Generic`
sums become `{tag, payload}` objects with coverage-checked case analysis.

Binders use parametric higher-order abstract syntax (PHOAS). A `let_` or a
lambda takes a Haskell function over the binder type `f`, and closed programs
are polymorphic in `f`, so a term with a free or captured variable does not
typecheck.

### Pure and effectful code

`Expr` and `Effect` are separate trees, and only `Effect` can mutate state,
touch the DOM, or call foreign code. Effectful code is written in do-notation
through `EffectSyntax`, which reifies each bind as an `Effect` node.

`Expr` can be evaluated as haskell values using `evaluate`, which is akin to constant folding. 
`Effect` programs can be tested tested by running the compiled output under
[Bun](https://bun.sh), with [happy-dom](https://github.com/capricorn86/happy-dom)
supplying `document` and `window` when a test needs them.

### Foreign code

`ffi` is the backdoor to JavaScript proper. The callee is
unchecked JavaScript text, the arguments are a typed record, and the result
type is whatever signature you give the wrapper. `jshark-bindgen` generates
these wrappers from TypeScript declaration files using the TypeScript
compiler's own type checker.

## Usage

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as BS
import JShark.Prelude
import qualified JShark.Console as Console

main :: IO ()
main =
  compileEffectSyntax readableConfig (Console.log "hello, world" >> done)
    >>= BS.putStrLn
```

Pure expressions can be evaluated directly:

```haskell
ghci> evaluateNumber ((number 10 + number 2) * number 4)
48.0
```

The [tutorial](docs/tutorial.md) covers control flow, the DOM, records and
sums, typed events, and the FFI.

## Demos

Four applications, rebuilt from `master` on every push and shown next to the
JavaScript they compile to: **<https://goolord.github.io/jshark/>**

| | |
| :--- | :--- |
| [**Breakout**](examples/src/JShark/Example/Breakout) | Canvas 2D game loop with collision detection and mutable state. |
| [**TodoMVC**](examples/src/JShark/Example/TodoMvc) | TodoMVC with `jshark-lucid` templates and `localStorage` persistence. |
| [**Synth**](examples/src/JShark/Example/Synth) | Polyphonic Web Audio synthesizer over hand-written typed bindings. |
| [**Life**](examples/src/JShark/Example/Life) | Conway's Game of Life, rendered with WebGL and stepped in a web worker. |

## Packages

| | |
| :--- | :--- |
| [**jshark**](packages/jshark) | The EDSL, host evaluator, and compiler. |
| [**jshark-lucid**](packages/jshark-lucid) | DOM construction using Lucid's element and attribute combinators. |
| [**jshark-bindgen**](packages/jshark-bindgen) | Generates typed Haskell bindings from TypeScript `.d.ts` files. |
| [**jshark-hotreload**](packages/jshark-hotreload) | File watcher and SSE middleware for live reloading in a dev server. |

`jshark-lucid` works because Lucid's `Term` and `With` are open classes, so
container elements and attributes are reused unchanged and compile to
`createElement` and `setAttribute` calls. Dynamic parts go in the child block:

```haskell
li_ $ do
  classWhen isDone "completed"
  voidWith_ "input" [type_ "checkbox"] $ on "click" toggle
```

## Building

With Nix, the dev shell provides the full toolchain:

```bash
git clone https://github.com/goolord/jshark.git
cd jshark
nix develop
cabal build all
```

Without Nix, you need **GHC 9.14+** and **Cabal 3.12+**. Outside Windows,
`cabal.project` builds this repository with the LLVM 20 backend, so `opt-20`
and `llc-20` must be on `PATH`. That setting is local to the repository, and
projects that depend on `jshark` do not need LLVM. [Bun](https://bun.sh) runs
the generated JavaScript in the test suites, and `jshark-bindgen` needs Bun
and the `typescript` package to read declaration files.

Serve the demos with live reloading at `http://localhost:3000`:

```bash
cabal run exe:jshark-examples
```

Export them as a static site, which is how the GitHub Pages build works:

```bash
cabal run exe:jshark-examples -- export ./dist
```

Run the tests:

```bash
cabal test all --test-show-details=direct
```

## Limitations

Some JavaScript is outside the subset on purpose:

- Classes, `this`, and prototype mutation are not modelled. Use `ffi` for
  APIs that require them.
- Type checking stops at the FFI boundary. A foreign call is only as correct
  as the signature written for it, and it can throw at runtime; `catch_`
  handles that.
- JShark does not resolve modules or bundle dependencies. Load npm packages
  with a script tag or your own bundler, then bind them with `ffi` or
  `jshark-bindgen`.

## Documentation

- [Tutorial](docs/tutorial.md): a guided tour of the API.
- [Benchmarking and testing](docs/benchmarking-and-testing.md): test suites, profiling, and RTS options.
- [Contributing](CONTRIBUTING.md): building, testing, and formatting before a PR.
- [Changelog](CHANGELOG.md)

## License

BSD-3-Clause. See [LICENSE](LICENSE).
