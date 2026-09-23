# jshark

Write typed JavaScript in Haskell. Generate code you can read.

JShark embeds a typed subset of JavaScript in Haskell. GHC checks your
program; JShark compiles it without a Haskell runtime. Haskell functions
represent binders, keeping typed terms free of unbound variables and
variable capture.

This package is the core: the typed AST (`JShark.Api.Types`), the EDSL
surface (`JShark.Api`, `JShark.Object`), the host evaluator, and the
compiler (`JShark`). Platform bindings, the IO build driver, and the Bun
runner live in [`jshark-base`](https://github.com/goolord/jshark/tree/master/packages/jshark-base).

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as BS
import JShark (effectfulProgram, renderJS)
import JShark.Api

greet :: Expr f 'String -> Effect f 'Unit
greet name = discard (ffi "console.log" (arg ("hello, " <> name) <: RecNil))

main :: IO ()
main = BS.putStrLn (renderJS (effectfulProgram (greet (string "world"))))
```

Evaluate pure expressions in GHCi without a JavaScript engine:

```haskell
ghci> import JShark (evaluateNumber)
ghci> import JShark.Api (number)
ghci> evaluateNumber ((number 10 + number 2) * number 4)
48.0
```

## Documentation

- [JShark tutorial](https://github.com/goolord/jshark/blob/master/docs/tutorial.md)
- [Full README](https://github.com/goolord/jshark#readme)
- [Live demos](https://goolord.github.io/jshark/)
