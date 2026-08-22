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

## vs Haskell / JavaScript

The host is Haskell. The runtime is a typed Good Parts subset of JS.
`evaluate` walks the tree in Haskell; codegen prints JS.

Binders are PHOAS, so host `case` and capture-avoiding substitution
work in Haskell. Emitted JS is strict. Host sharing
(`let x = e in x + x`) is recovered only by `evaluateCached`
(StableName memo), not by `evaluate`. `do` is `EffectSyntax`
(statements), not `IO`. `Maybe` / `Either` are not ADTs in JS: they
become `Option` / `Result` encodings (below). You eliminate them with
`optionCase` / `resultCase`, not Haskell `case`. Generic sums are
`{tag, payload}` objects; `caseSum` is an `if (s.tag === "Ctor")`
chain. `CaseEnd` throws on leftovers. Named arms must be a prefix of
declaration order.

Numbers follow JS, not Prelude. `rem_` is `%` (sign of the dividend),
not `mod`. Bitwise ops use ToInt32. `Math.round` is `floor(x + 0.5)`
(half toward +Infinity): `round 2.5` is `2` in Haskell and `3` here.
`Int` is IEEE `Number`; `fromValue` truncates. `.==` / `.!=` emit
`$eq` / `!$eq` (`===` then structural arrays and plain objects).
Only numbers, strings, and bools are `Comparable`, not objects.

No `==`, `with`, `eval`, `this`, or implicit `new`. Functions stay
unary: `lambda2` is `function(x){ return function(y){ return x + y } }`,
not `function(x, y)`. `Array.sort`'s compare is the exception (a
binary JS callback). `parseInt_` requires a radix. Regex is
`new RegExp("src")`, not `/src/`. `stringCaseE` is `switch` with no
fall-through; first label wins. `&&` / `||` short-circuit. Array
index is `Math.trunc`; out of bounds is a hard error, not
`undefined`. Frozen `'Object r` is an `Expr` (pure field get).
`'MutableObject` is an `Effect`. Named stdlib is a closed constructor
set; free-text names live on `Effect` FFI.

`Option` is not a JS type. `none` is `null`; `some x` unwraps to `x`.
`typeof null` is `"object"`. You cannot unwrap with `if_` plus a
coerce. `optionCase` is a primitive (`n === null ? none : some`). A
non-literal `some` is `UnsafeNullable`: the interpreter treats it as
`Some`; a real `null` comes from `none` or FFI (`Storage.getItem`,
`Canvas.getContext2d`).

`Result` is not a JS built-in. It is `{ok: true, value: v}` /
`{ok: false, value: e}`. `ok` of `Unit` is `{ok: true, value: undefined}`.

## Option, Result, products, sums

Host `Maybe` is `Option`. Host `Either` is `Result` and stays
`Result` inside a Generic record.

```haskell
import JShark.Api
import qualified JShark.Generic as G

-- Option: none → null, some unwraps
orElse (none :: Expr f ('Option 'String)) (string "nobody")
-- "nobody"

optionCase (some (string "hi")) (string "nobody") id
-- "hi"

-- Result: {ok, value}
ok (number 5) :: Expr f ('Result 'String 'Number)
-- {ok: true, value: 5.0}

err (string "e") :: Expr f ('Result 'String 'Number)
-- {ok: false, value: "e"}

resultCase (ok (number 5)) (\_ -> number 0) (\x -> x + 1)
-- 6
```

```js
n === null ? "nobody" : n
r.ok ? r.value + 1 : 0
```

`optionCaseE` and `resultCaseE` are the effectful eliminators.
`whenSomeE` runs a block only on `Some`. `fromOption` is
`flip orElse`.

**Product types** are Generic records as JS objects. Row `As a` (or
`MutableObjectOf a`). Field names are the Haskell selectors. Nested
`Maybe` is `Option` (`null` / the value). Nested products become
nested objects. Nested sums use `toSum`.

```haskell
data Person = Person { fullName :: Text, years :: Double }
  deriving Generic

data Card = Card
  { label :: Text
  , tags :: [Text]
  , nickname :: Maybe Text
  }
  deriving Generic

G.toObject (Person "Ada" 36)
-- {"fullName": "Ada", "years": 36.0}

G.toObject (Card "x" ["a"] Nothing)
-- {"label": "x", "tags": ["a"], "nickname": null}

G.newRecord @Person
-- {}

-- get @"fullName" o  or, with OverloadedRecordDot, o.fullName
```

**Sum types** are Generic ADTs as `{tag, payload?}`. Row `Tagged a`.
Nullary constructors omit `payload`. Unary payload is the value.
n-ary payload is a positional object (`"0"`, `"1"`).

```haskell
data Color = Red | Green | Blue
  deriving Generic

data Shape = Circle Double | Rect Double Double
  deriving Generic

G.toSum Red
-- {"tag": "Red"}

G.toSum (Circle 1.5)
-- {"tag": "Circle", "payload": 1.5}

G.toSum (Rect 2 3)
-- {"tag": "Rect", "payload": {"0": 2.0, "1": 3.0}}

G.caseSum shape $
  G.on @"Circle" (\r -> expr r) $
  G.on @"Rect"   (\_ -> expr (number 0)) $
  G.CaseEnd

-- one arm; miss is the else
G.whenTag @"Red" color (\_ -> expr (string "yes")) (expr (string "no"))
```

```js
if (s.tag === "Circle") { s.payload }
else if (s.tag === "Rect") { 0 }
else { throw "JShark.Generic: caseSum: unhandled " + s.tag; }
```

`G.toObjectArray` / `G.toSumArray` map those over a Haskell list.
Other stdlib is `JShark.*`.

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
