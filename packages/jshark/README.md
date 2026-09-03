# JShark

A JavaScript EDSL embedded in Haskell.

JShark programs are ordinary Haskell values: the object language is
JavaScript, the host is Haskell, and the embeddable "pure" subset is typed and
modeled on the Good Parts. The subset is restricted enough to take advantage of Haskell's strong type system, but expressive enough to interact with existing JavaScript code and write real, fast web applications.

Binders are higher-order using PHOAS: a lambda is a Haskell function, so a term
can never name something that is not in scope, substitution is just function
application, and capture is impossible. Statements are compose with `do`
notation, and literals and operators use the standard Haskell classes, which means writing JShark feels like writing Haskell.

JShark started as an answer to "the JavaScript problem" for the band of
applications that are more complex than a static site but not complex enough
to justify using GHCJS.

Live examples (built from `master` by GitHub Actions):
<https://goolord.github.io/jshark/>
it may be useful to inspect the haskell source code (viewable in this repository), and then the `Readable` output (available in the details pane of each example).

## Features

- **JavaScript you can read.** Codegen keeps the program's shape and names
  instead of emitting a blob: a `let`-bound temporary is named after the
  Haskell function that created it (recovered via `HasCallStack`), and
  function arguments keep the Haskell binder names they were written with,
  so a two-argument callback written `\(a :: Expr f 'Number) (b :: Expr f 'Number) -> a + b`
  compiles to `(a, b) => a + b`. `readableConfig` prints a pretty snippet
  for debugging; the default config emits a minified IIFE for production.
  
- **FFI that's gradually typed.** When the core does not cover a call, `ffi`
  embeds it and the arguments stay type-checked `Expr`s, literals included:
  `ffi "console.log" (arg "max" <: arg 2 <: arg 9 <: RecNil)`. Method calls
  are `callMethod el "setAttribute" args`; an argument that is itself an
  effectful call is passed with the `ArgEffect` constructor. although `ffi` is polymorphic, you can statically type ffi calls by monomorphizing the type of a halper function that calls `ffi` (see `examples/synth/AudioParam.hs` for a real example).

- **Two typed abstract syntax trees.** The API is split into a pure expression AST and an
  effectful program AST. Pure code (literals, functions, arrays, objects,
  `Option`/`Result`) is a strict subset you can reason about; effects
  (statements, mutation, DOM, I/O, FFI) form a separate tree that joins the
  pure one only at typed seams.

- **One term, two runtimes.** A pure expression can be evaluated in Haskell
  (fast tests, no JS runtime) or compiled to JavaScript, and the test suite
  compiles programs and checks the emitted JavaScript against the
  interpreter and against `bun`. Effectful programs run under `bun`,
  optionally with browser globals from happy-dom, so DOM and storage code is
  testable without a browser.

- **The host language is the macro system.** Libraries are just Haskell.
  Combinators like `map`, `zipWith`, and `groupBy` are Haskell functions
  that build JavaScript arrays. `Generic` records and sums become JS
  objects (`{"fullName": "Ada"}` and `{"tag": "Circle", "payload": ...}`),
  and `OverloadedRecordDot` reads object fields.

- **Real programs.** The repository ships five applications whose client
  logic is written in JShark and compiled to the JavaScript served on the
  site, from TodoMVC to a Web Audio synthesizer to a WebAssembly/HVM2 lab.

## Limitations

- **A subset by design.** Idiomatic JS that falls outside the core (classes,
  `this`, prototype tricks, dynamic member access) must go through `ffi`,
  which is unchecked free text: a typo surfaces as broken or misbehaving
  JavaScript, not a compile error. The tests parse-check every emitted
  program to catch this.

- **JavaScript semantics leak through.** The object language is JS, not
  Haskell: numbers are IEEE doubles (bitwise ops are ToInt32), exact
  integers are a separate `BigInt` type, and `Maybe`/`Either` become JS
  `Option` (`null`/value) and `Result` (`{ok, value}`). Default semantics are meant to be familiar to Haskell programmers, but intrinsic types like `Number` and `String` should be expected to behave like their JS counterparts.

- **Runtime errors remain possible.** Type safety covers the subset's
  structure, not the platform: an `ffi` call can fail, a wrapped API can
  throw, and JS exceptions still exist (`catch_` is available).

- **Not a drop-in JS replacement.** There is no way to link against an
  arbitrary npm library directly, but you can generate wrappers using
  `jshark-bindgen` or reaching for `ffi` directly.

- **A real toolchain.** Current builds need GHC 9.14. Some features want optional tools (`bun` for the JS-vs-interpreter tests and effect evaluation, `zig` for the HVM2 WASM, `esbuild` for the synth). A Nix shell pins the whole toolchain (see flake.nix for a full list of tools).

## Quick start

A closed program is a value whose type is `forall f. Effect f 'Unit`. This
one logs a greeting; compiling it prints the JavaScript:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.IO as T
import JShark.Api
import JShark.Compiler
import qualified JShark.Console as Console

greet :: Expr f 'String -> Effect f 'Unit
greet name = fromSyntax $ do
  Console.log ("hello, " <> name)
  done

main :: IO ()
main = compileEffect readableConfig (greet "world") >>= T.putStrLn
```

```js
console.log("hello, world");
```

Pure terms are just as easy to try in a REPL, with no JavaScript involved:

```
> import JShark (evaluateNumber)
> import JShark.Api (number)
> evaluateNumber ((number 1 + number 2) * number 4)
12.0
```

### Arbitrary JavaScript with `ffi`

The typed core does not cover every call. `ffi` embeds a call whose callee
name is free text, while each argument stays a type-checked `Expr`, string
and numeric literals included:

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

```js
console.log("max", 2, 9);
```

The callee name is emitted verbatim, so a typo there is on you. Method calls
are the same idea, with an object handle in front:
`callMethod el "setAttribute" (arg "k" <: arg v <: RecNil)` renders
`el.setAttribute("k", v)`. An argument that is itself the result of an
effectful call uses the `ArgEffect` constructor instead of `arg`.

More working programs live in the `examples` package (`examples/src/`, each app
is a `JShark.Example.*` module) and in the package test suites, which show a
great deal of what compiles to what.

## Example programs

The `jshark-examples` executable (in the `examples` package) compiles and
serves five programs; `/` lists them:

- `breakout` - a Canvas Breakout clone
- `todo-mvc` - the TodoMVC app, using `jshark-lucid`
- `synth` - a polyphonic Web Audio synthesizer; `AudioParam` automation keeps
  timing on the audio thread, and the Web Audio API is bound behind typed
  `ffi` wrappers so the instrument code stays in the safe subset
- `life` - Conway's Game of Life with WebGL rendering (PixiJS) and a
  worker-based engine
- `hvm2-demo` - a Mandelbrot zoom lab that compares a JS reference, a WASM
  SIMD grid, and HVM2 net reduction

## Related packages in this repository

`jshark-lucid` describes DOM in Lucid syntax and compiles it to
`createElement` calls, so a template lives in one place rather than being
spelled out as imperative JavaScript. Condensed from the TodoMVC example:

```haskell
li_ $ do
  classWhen isDone "completed"
  voidWith_ "input" [type_ "checkbox"] $ on "click" toggle
```

`jshark-bindgen` is a separate executable (not part of the `jshark`
library). It reads a TypeScript declaration file, or JavaScript with JSDoc,
and prints a Haskell module of typed `ffi` wrappers:

```
cabal run jshark-bindgen -- lib.d.ts --module JShark.Lib
```

Full TypeScript extraction uses `bun` and the `typescript` package (see
`packages/jshark-bindgen/package.json`); `--no-ts` falls back to a built-in
Haskell `.d.ts` parser.

## Building and installing

Clone the repository and build with cabal:

```
git clone https://github.com/goolord/jshark.git
cd jshark
cabal build
```

Requirements:

- GHC 9.14 and a current cabal (CI uses GHC 9.14.1 / cabal 3.12).
- LLVM 20 (`opt-20`, `llc-20`) on PATH on non-Windows systems.
- `bun` on PATH for the parts of `cabal test` that run the generated JS and
  for `JShark.Bun.evaluateEffectJSON`.

Run the tests and the example server:

```
cabal test all       # JS-vs-interpreter checks need bun on PATH
cabal run exe:jshark-examples  # serves the examples at http://localhost:3000
```

`cabal run exe:jshark-examples -- export DIR` writes a static copy of the site,
which is how GitHub Pages is updated on `master`. `scripts/check-wasm.sh`
rebuilds the vendored HVM2 WASM and compile-checks the Life example's Zig
kernels.

A Nix shell pins the whole toolchain (GHC, cabal, LLVM 20, bun, zig, Biome,
Fourmolu, esbuild): `nix develop`.
