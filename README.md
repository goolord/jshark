## 🦈 JShark
https://goolord.github.io/jshark/

Haskell EDSL that emits JavaScript. Binders are PHOAS: a variable is
`f u` for some `f :: Universe -> Type`, and a closed term is `forall f`.
A term cannot mention a name that is not in scope, and substitution is
ordinary Haskell function application, so you get capture avoidance
without a name supply. The same tree can be evaluated, optimized, and
compiled without renaming anything by hand.

There are two trees. `Expr` is pure: literals, arithmetic, `.==` / `.!=`,
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

`ffi` is a free-text call. Arguments are a `Rec` of `Arg`: `arg` wraps
an `Expr`; `ArgEffect` passes an `Effect` without lifting it into `Expr`.

```haskell
import JShark.Api
import JShark.Rec (Rec (..), (<:))

sumLog = fromSyntax $ do
  toSyntax_ $
    ffi
      "console.log"
      ( arg (string "max")
          <: arg (number 2)
          <: arg (number 9)
          <: RecNil
      )
  done
```

```js
console.log("max", 2.0, 9.0);
```

```
cabal build
cabal test          # bun on PATH for the JS-vs-interpreter checks
cabal run examples  # http://localhost:3000
```

`jshark-lucid` writes the DOM in Lucid and compiles it to
`createElement` calls, so a template lives in one place instead of being
spelled out as imperative JS. Lucid's containers and attributes are
reused as-is; the dynamic parts are children.

```haskell
import JShark.Lucid
import Lucid (button_, class_, div_, label_, li_, type_)

todoItem title isDone toggle = li_ $ do
  classWhen isDone "completed"
  div_ [class_ "view"] $ do
    voidWith_ "input" [class_ "toggle", type_ "checkbox"] $ do
      prop "checked" isDone
      on "click" toggle
    label_ (dynText title)
    button_ [class_ "destroy"] mempty
```

```js
const n1 = document.createElement("li");
n1.classList.toggle("completed", n0.completed);
// …createElement("div"), setAttribute, appendChild…
```

`examples/` is TodoMVC and Breakout as named libraries, served together.
`/` lists them. After the Pages workflow on `master`: https://goolord.github.io/jshark/.
`test/Main.hs` has more of what compiles to what.

## vs Haskell / JavaScript

The host is Haskell; the object language is a typed Good Parts subset
of JS. `evaluate` walks a pure `Expr`; `evaluateEffectJSON` runs an
`Effect` with `bun` and returns its JSON. Codegen prints JS.

Bare bun has no `document`. Pass `domBunConfig` to run an `Effect`
against browser globals (happy-dom, registered in-process), which is
what makes `JShark.Dom` and `JShark.Storage` testable:

```haskell
import JShark.Bun

-- <div id="a"></div>, then setInnerText "hello", then read it back
evaluateEffectJSONWith
  domBunConfig {bunEnv = HappyDom defaultHappyDomOptions {happyDomBody = "<div id=\"a\"></div>"}}
  effect
-- "\"hello\""
```

`do` is `EffectSyntax`. `Maybe` and `Either` are `Option` and
`Result` (below) — eliminate them with `optionCase` / `resultCase`,
not host `case`. Numbers are IEEE `Number`: `rem_` is `%`, bitwise
is ToInt32, and `Math.round` is half toward +Infinity (`2.5` → `3`).

No `==`, `with`, `eval`, `this`, or implicit `new`. No `/src/`
literals (`new RegExp`). Functions are unary (`Array.sort`'s compare
is the binary exception). Array index is `Math.trunc` and throws on
OOB. `parseInt_` takes a radix. `.==` is `$eq` (`===`, then
structural arrays and plain objects). Frozen `'Object` is `Expr`;
`'MutableObject` is `Effect`.

`JShark.Classes` copies `Functor` / `Monad` / `Foldable` / … at kind
`Universe -> Universe` (object-language maps). Import qualified; they
are not Prelude. `Semigroup` / `Monoid` on `Expr` *are* the `base`
classes (`Option` combines innards like `Maybe`). `foldr` is
`Array.prototype.reduceRight`.

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
