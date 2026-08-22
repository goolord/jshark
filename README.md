## 🦈 JShark
https://goolord.github.io/jshark/

Haskell EDSL that emits JavaScript. Binders are PHOAS: a variable is
`f u` for some `f :: Universe -> Type`, and a closed term is `forall f`.
A term cannot mention a name that is not in scope, and substitution is
ordinary Haskell function application, so you get capture avoidance
without a name supply. The same tree can be evaluated, optimized, and
compiled without renaming anything by hand.

There are two trees. `Expr` is pure: literals, arithmetic, `===` / `!==`,
functions, `const` lets, and a fixed set of string, array, Math, and
JSON methods. `Effect` is impure: statements, FFI, mutation, DOM, I/O,
and free-text names. They join at FFI through `Arg`, so you can hand an effect
to FFI without first lifting it into `Expr`. The optimizer can fold `Expr` without
pretending FFI is pure, and codegen can print expressions and statements
differently.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.IO as T
import JShark.Api
import JShark.Compiler
import qualified JShark.Console as Console

greet name = fromSyntax $ do
  Console.log ("hello, " <> name)
  done

main = compileEffect readableConfig (greet "world") >>= T.putStrLn
```

```js
console.log("hello, world");
```

```
cabal build
cabal test          # bun on PATH for the JS-vs-interpreter checks
cabal run examples  # http://localhost:3000
```

`examples/` is TodoMVC and Breakout as named libraries, served together.
`/` lists them. After the Pages workflow on `master`: https://goolord.github.io/jshark/.
`test/Main.hs` has more of what compiles to what.

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
