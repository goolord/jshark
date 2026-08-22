## 🦈 JShark

Haskell EDSL that emits JavaScript. Two PHOAS trees: pure `Expr`, impure
`Effect`, and a closed term is `forall f`. The JS is Crockford's Good
Parts: `===` / `!==`, `const`, and none of `==`, `with`, or `eval`. FFI
takes an `Arg` (expression or effect); raw names live on `Effect`.

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
cabal run todo-mvc  # http://localhost:3000
```

`app/todo-mvc` is the example. `test/Main.hs` has more of what compiles
to what.

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
