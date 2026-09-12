# jshark

Write typed JavaScript in Haskell. Generate code you can read.

JShark embeds a typed subset of JavaScript in Haskell. GHC checks your
program; JShark compiles it without a Haskell runtime. Haskell functions
represent binders, keeping typed terms free of unbound variables and
variable capture.

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as BS
import JShark.Prelude
import qualified JShark.Console as Console

greet :: Expr f 'String -> Effect f 'Unit
greet name = fromSyntax $ do
  Console.log ("hello, " <> name)
  done

main :: IO ()
main = compileEffect readableConfig (greet (string "world")) >>= BS.putStrLn
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
