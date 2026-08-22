## 🦈 JShark

JShark is a typed subset of javascript: a **pure** PHOAS expression tree (`Expr`) and an **impure** effect tree (`Effect`). Closed terms are `forall f` (Kmett's PHOAS end). FFI arguments are `Arg` (expression or effect); method calls are `CallMethod`. `UnsafeEffectExpr` is an optimizer splice, not the FFI path. Equality is `===` / `!==`; bindings are `const`.
The goals are as follows:
- Have a haskelly user interface
- Have an easy to use ffi
- Compile to idiomatic JavaScript

Codegen constant-folds literals and drops dead pure bindings, then inlines single-use bindings (so you don't get `const n0 = x; n0` everywhere). For a pretty snippet, compile with `readableConfig`. For production, `defaultCompilerConfig` wraps an IIFE and minifies. The default backend is [esbuild](https://esbuild.github.io) — much faster than Google Closure Compiler, and the right default in 2026. Closure is still available if you want its `ADVANCED` whole-program renaming (you'll need externs for DOM/FFI). Terser is there too.

```haskell
import qualified Data.Text.IO as TIO
import JShark.Api (number)
import JShark.Compiler

main :: IO ()
main = do
  pretty <- compilePure readableConfig (number 1 + number 2)
  minified <- compilePure defaultCompilerConfig (number 1 + number 2)
  TIO.putStrLn pretty
  TIO.putStrLn minified
```

`compilePure readableConfig` / `compileEffect readableConfig` emit a human-readable snippet: single-use lets and effect binds are inlined, nothing is wrapped in an IIFE, and minifiers are skipped (`OutputStyle` `Readable` forces `Passthrough`). `Minified` (the default) wraps an IIFE so a minifier cannot dead-code-eliminate the result, then runs the configured backend. `compileJS` does not wrap: a bare expression can minify to empty. Results are cached in memory by default (`MemoryCache`, capped); pass `DiskCache dir` or `NoCache` via `compileWith`. Named backends (`compileEsbuild` etc.) throw if the tool is missing; `defaultCompilerConfig` (`Auto`) logs to stderr and returns the unminified source. Use `tryCompileWith` for an `Either`. `nix develop` puts `esbuild` and `bun` on `PATH`.

### Building

JShark builds with a modern GHC. just `cabal build` / `cabal test`.
Tests that check generated JS against the reference interpreter need [bun](https://bun.sh) on `PATH`. If it is missing, the `bun is on PATH` check fails and the rest of that group is skipped. `nix develop` puts `esbuild` and `bun` on `PATH`.

### TodoMVC demo

```
cabal run todo-mvc
```

Opens a classic TodoMVC UI on [http://localhost:3000](http://localhost:3000): Scotty serves Lucid HTML, and the client behaviour is compiled from JShark (`app/todo-mvc`).

### Status

The core language currently supports:
- Arithmetic, string, and boolean expressions (`+`, `-`, `*`, `/`, `&&`, `||`, `===`, comparisons on numbers/strings/bools, `Show`/`typeof`, string `<>`)
- `let`, lambdas/application, and a ternary conditional (`if_`)
- `Option` with `optionCase` / `optionCaseE`; `some`/`none` wrap a nullable cast and JS `null`
- Effectful statements: `EffectSyntax` (`yield`, `hold`), `forEach`, `ifE`/`when_`/`while_` (pass `expr c` or `ffi "cond" RecNil`), `try_`, and `ffi` / `callMethod`
- Typed records: `Field` + `get @` / `set @` / `newObject`. Untyped FFI blobs use `getProp`/`setProp`
- Named stdlib on `Expr` is closed-name (`toUpper`, `length_`, `stringify`, `map_`, `filter_`, `Array.index`, `Math.*`). True escapes (`alert`, raw `foo()`, free-text methods) use `ffi` / `callMethod` on `Effect`. `JSON.parse` is `unsafeParse` / `tryParse` on `Effect`.

Stdlib/browser wrappers built on top of the core language:
- `JShark.Array`: `index`, `length_`, `map_`/`mapE`, `filter_`/`filterE`, `includes`, `concat_`, `join`, `push`
- `JShark.String`: `length_`, `indexOf`, `slice`, `toUpper`, `toLower`, `trim`, `split`, `replace`
- `JShark.Math`: constants (`pi`, `e`, ...) plus `sin`/`cos`/`tan`/`sqrt`/`pow`/`atan2`/`max_`/`min_`/`random`/etc.
- `JShark.Json`: `stringify` (`Expr`), `unsafeParse` / `tryParse` (`Effect`)
- `JShark.Console`: `log`, `warn`, `error_`, `info`
- `JShark.Storage`: `localStorage`/`sessionStorage`, `getItem`, `setItem`, `removeItem`, `clear`
- `JShark.Timers`: `setTimeout`, `setInterval`, `clearTimeout`, `clearInterval`
- `JShark.Promise`: `promiseThen`, `promiseCatch`
- `JShark.Dom`: element lookup/creation (`lookupId`, `lookupSelector`, `createElement`), tree manipulation (`appendChild`, `removeChild`), attributes/classes/content
- `JShark.Ajax`: `XMLHttpRequest` wrapper and minimal `fetch`

See `JShark.Example` for a short end-to-end sample, and `test/Main.hs` for
more examples of what compiles to what.
```
                   /""-._
                  .      '-,
                  :         '',           _________________________________
                  ;      *     '.       /                                /
                  ' *         () '.    / bark bark what's for lunch lol /
                   \               \  /________________________________/
                    \      _.---.._ '.
                     :  .' _.--''-''  \ ,'
       .._            '/.'             . ;
        ; `-.          ,                \'
         ;   `,         ;              ._\
          ;    \     _,-'                ''--._
           :    \_,-'                          '-._
            \ ,-'                       .          '-._
           .'         __.-'';            \...,__       '.
          .'      _,-'       \              \   ''--.,__ '\
         /   _,--' ;          \             ;           "^.}
        ;_,-' )     \  )\      )            ;
             /       \/  \_.,-'             ;
            /                              ;
         ,-'  _,-'''-.    ,-.,            ;
      ,-' _.-'        \  /    |/'-._...--'
     :--``             )/
```
