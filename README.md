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
Crockford's *JavaScript: The Good Parts*. Pure and effectful code is split
in `Expr f (u :: Universe)` and `Effect f (u :: Universe)` respectively.
The jshark compiler lowers the typed AST to idiomatic JavaScript with no runtime library.
Standard library functions that make the language more sane (structural equality, `groupBy`) 
are printed in a preamble only when a program uses them.

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

| Haskell Type (`Universe`) | JavaScript Runtime Type / Structure | Description & Semantics |
| --- | --- | --- |
| `Number` | `number` | IEEE 754 64-bit float with 32-bit bitwise operations |
| `BigInt` | `bigint` | Arbitrary-precision integer |
| `String` | `string` | UTF-16 text string |
| `Bool` | `boolean` | `true` or `false` |
| `Unit` | `undefined` / `null` | Unit / void value |
| `Array u` | `Array` (`u[]`) | Standard JavaScript array |
| `Function u v` | `(u) => v` | Unary function; chained/nested to support currying |
| `Fn us v` | `(...us) => v` | Uncurried native $n$-ary JavaScript function |
| `Option u` | `{ some, value }` | Tagged object instead of `null` to ensure nested options (e.g., `Some(None)`) remain distinct |
| `Result e a` | `{ ok, value }` | Tagged object representing success (`ok: true`) or failure (`ok: false`) |
| `Map k v` | `Map` | Standard JavaScript key-value map |
| `Set u` | `Set` | Standard JavaScript set |
| `Uint8Array` | `Uint8Array` | Typed byte array buffer view |
| Row-typed objects | `Object` | Frozen (`Object.freeze`) or mutable key-value object |
| `Generic` records | Plain `Object` | Record fields mapped directly to object properties |
| `Generic` sums | `{ tag, payload }` | Tagged union with coverage-checked pattern matching |

### Pure and effectful code

`Expr` and `Effect` are separate trees, and only `Effect` can mutate state,
touch the DOM, or call foreign code. Effectful code is written in do-notation
through `EffectSyntax`, which reifies each bind as an `Effect` node.

`Expr` can be evaluated as haskell values using `evaluate`, which is akin to constant folding. 
`Effect` programs can be tested tested by running the compiled output under
[Bun](https://bun.sh), with [happy-dom](https://github.com/capricorn86/happy-dom)
supplying `document` and `window` when a test needs them.

### FFI

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

**<https://goolord.github.io/jshark/>**
each demo contains the compiled JavaScript in the human readable output format

| | |
| :--- | :--- |
| [**Breakout**](examples/src/JShark/Example/Breakout) | Canvas 2D game loop with collision detection and mutable state. |
| [**TodoMVC**](examples/src/JShark/Example/TodoMvc) | TodoMVC with `jshark-lucid` templates and `localStorage` persistence. |
| [**Synth**](examples/src/JShark/Example/Synth) | Polyphonic Web Audio synthesizer over hand-written typed bindings. |
| [**Life**](examples/src/JShark/Example/Life) | Conway's Game of Life, rendered with WebGL and stepped in a web worker. |

## Packages

| | |
| :--- | :--- |
| [**jshark**](packages/jshark) | The typed AST, the host evaluator, and the compiler. |
| [**jshark-base**](packages/jshark-base) | The EDSL surface and the one-import `JShark` module, platform bindings (arrays, maps, DOM, canvas, timers, …), `Generic` encodings, the IO build driver, and the Bun runner. |
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

```bash
git clone https://github.com/goolord/jshark.git
cd jshark
nix develop
cabal build all
```

Requirements:
- [Bun](https://bun.sh) runs the generated JavaScript in the test suites + `jshark-bindgen`
- `llc-20` to build with `cabal.project`
- `typescript` for `jshark-bindgen`

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

## Documentation

- [Tutorial](docs/tutorial.md): a guided tour of the API.
- [Benchmarking and testing](docs/benchmarking-and-testing.md): test suites, profiling, and RTS options.
- [Contributing](CONTRIBUTING.md): building, testing, and formatting before a PR.
- [Changelog](CHANGELOG.md)

## License

BSD-3-Clause. See [LICENSE](LICENSE).
