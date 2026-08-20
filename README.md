## 🦈 JShark

JShark is a typed subset of javascript.
The goals are as follows:
- Have a haskelly user interface
- Have an easy to use ffi
- Compile to idiomatic JavaScript

ATM JShark is usable, but I would recommend running the output through [Google Closure Compiler](https://github.com/google/closure-compiler) in order to produce idiomatic JavaScript

### Building

JShark builds with a modern GHC. just `cabal build` / `cabal test`.

### Status

The core language currently supports:
- Arithmetic, string, and boolean expressions (`+`, `-`, `*`, `/`, `&&`, `||`, comparisons, `Show`/`String()` casting)
- `let`, lambdas/application, and a ternary conditional (`if_`)
- `Option`/`Result` types with `optionCase`/`resultCase` eliminators (`maybe`/`either`-style)
- Effectful statements: `let`-style binding (`EffectSyntax`), `forEach`, effectful conditionals (`ifE`/`when_`) and `while_` loops, and an escape hatch FFI for calling into arbitrary JS
- A small typed object/property-access mechanism (`Field`) used by the `Dom` and `Ajax` modules
- Generic escape hatches for calling arbitrary JS from pure expressions
  (`exprFfi`, `exprProp`, `exprMethod`, `exprMethodCallback`, `exprIndex`),
  and for embedding an effect (e.g. a callback) inside a pure expression
  (`unsafeEffectExpr`)

Stdlib/browser wrappers built on top of the core language:
- `JShark.Array`: `index`, `length_`, `map_`, `filter_`, `includes`, `concat_`, `join`, `push`
- `JShark.String`: `length_`, `indexOf`, `slice`, `toUpper`, `toLower`, `trim`, `split`, `replace`
- `JShark.Math`: constants (`pi`, `e`, ...) plus `sin`/`cos`/`tan`/`sqrt`/`pow`/`atan2`/`max_`/`min_`/`random`/etc.
- `JShark.Json`: `stringify`, `unsafeParse`
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
