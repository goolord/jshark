<div align="center">

# 🦈 JShark

**Write JavaScript in Haskell. Read the JavaScript it writes.**

[![CI](https://github.com/goolord/jshark/actions/workflows/ci.yml/badge.svg)](https://github.com/goolord/jshark/actions/workflows/ci.yml)
[![Live Demos](https://img.shields.io/badge/demos-goolord.github.io%2Fjshark-059669?logo=githubpages)](https://goolord.github.io/jshark/)
[![GHC 9.14+](https://img.shields.io/badge/GHC-9.14+-8f4e8b?logo=haskell)](https://www.haskell.org/)
[![License: BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE)

</div>

---

JShark is a Haskell library for building JavaScript programs. You write ordinary
Haskell values, GHC type-checks them, and JShark emits JavaScript you would be
happy to open in a browser's devtools.

```haskell
greet :: Expr f 'String -> Effect f 'Unit
greet name = fromSyntax $ do
  Console.log ("hello, " <> name)
  done
```

```javascript
console.log("hello, world");
```

That is the whole idea: no runtime, no bundler, no generated symbol soup — just
a typed subset of JavaScript (roughly Crockford's *Good Parts*) that happens to
be written in Haskell.

## Why you might want this

- **The output is readable.** Your binder names survive. Turn on `readableConfig`
  and the emitted code looks hand-written, which makes debugging in the browser
  feel normal.
- **Your editor already knows the language.** JShark programs are Haskell values,
  so you get real types, real refactoring, and real reuse. A button is a
  function. A widget is a combinator.
- **Effects are visible in the type.** `Expr` is pure computation; `Effect` is
  anything that touches the world. You can tell them apart at a glance, and the
  compiler keeps them apart.
- **You can test without a browser.** Pure expressions evaluate directly in
  Haskell. Effectful programs run under Bun with `happy-dom` when you need the
  real thing.
- **Scope is safe by construction.** Binders are Haskell functions (PHOAS), so
  unbound variables and accidental capture are not expressible.

## Try it

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

Pure expressions don't even need that much — they evaluate in GHCi:

```haskell
ghci> evaluateNumber ((number 10 + number 2) * number 4)
48.0
```

The [tutorial](docs/tutorial.md) picks up from here: control flow, the DOM,
records and sums, typed events, and calling out to JavaScript you didn't write.

## Demos

Four apps, built from `master` on every push, each with its Haskell source
beside the JavaScript it produced: **<https://goolord.github.io/jshark/>**

| | What it shows |
| :--- | :--- |
| [**Breakout**](examples/src/JShark/Example/Breakout) | A canvas game loop, collision physics, and mutable state. |
| [**TodoMVC**](examples/src/JShark/Example/TodoMvc) | Declarative DOM with `jshark-lucid`, plus local storage. |
| [**Synth**](examples/src/JShark/Example/Synth) | A polyphonic Web Audio synth over hand-written typed bindings. |
| [**Life**](examples/src/JShark/Example/Life) | Conway's Life on WebGL, stepped in a web worker. |

## The packages

| | |
| :--- | :--- |
| [**jshark**](packages/jshark) | The language and the compiler. Start here. |
| [**jshark-lucid**](packages/jshark-lucid) | Write the DOM with Lucid's syntax; get `createElement` calls. |
| [**jshark-bindgen**](packages/jshark-bindgen) | Turn TypeScript `.d.ts` files into typed Haskell bindings. |
| [**jshark-hotreload**](packages/jshark-hotreload) | Live reloading for a dev server: a file watcher and SSE middleware. |

A taste of `jshark-lucid`, which reuses Lucid's combinators as-is:

```haskell
li_ $ do
  classWhen isDone "completed"
  voidWith_ "input" [type_ "checkbox"] $ on "click" toggle
```

## Getting started

The shortest path is Nix, which pins the whole toolchain for you:

```bash
git clone https://github.com/goolord/jshark.git
cd jshark
nix develop
cabal build all
```

Without Nix you'll want **GHC 9.14+** and **Cabal 3.12+**. Outside Windows
this repository also builds through **LLVM 20**, so `opt-20` and `llc-20`
need to be on your `PATH` — that comes from `cabal.project`, not from the
library, so a project that merely *depends* on `jshark` doesn't need it.
[Bun](https://bun.sh) is what runs the generated JavaScript in the tests,
and `jshark-bindgen` needs it (plus the `typescript` package) to read
TypeScript at all.

Run the demos locally with live reloading at `http://localhost:3000`:

```bash
cabal run exe:jshark-examples
```

That same binary writes the static site you see on GitHub Pages, if you want
to host the demos yourself:

```bash
cabal run exe:jshark-examples -- export ./dist
```

And the tests:

```bash
cabal test all --test-show-details=direct
```

## Good to know

JShark is a *subset* of JavaScript, and that's the point. Some things are
deliberately outside it:

- **Classes, `this`, and prototype mutation** aren't modelled. Reach for `ffi`
  when you need them.
- **Numbers are JavaScript numbers** — IEEE 754 doubles, with 32-bit bitwise
  operations. Use `BigInt` when you need exact integers.
- **Types check your program, not the web.** A foreign call can still throw;
  `catch_` is there for when it does.
- **It isn't a bundler.** npm packages come in through a script tag or your own
  bundler, and you bind them with `ffi` or `jshark-bindgen`.

## Docs

- [Tutorial](docs/tutorial.md) — the guided tour.
- [Benchmarking & testing](docs/benchmarking-and-testing.md) — test suites, profiling, RTS knobs.
- [Contributing](CONTRIBUTING.md) — how to build, test, and format before you open a PR.
- [Changelog](CHANGELOG.md) — what's changed.

## License

BSD-3-Clause. See [LICENSE](LICENSE).
