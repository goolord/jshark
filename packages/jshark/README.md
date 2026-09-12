# jshark

> A typed, readable JavaScript EDSL embedded in Haskell.

JShark programs are ordinary Haskell values: the object language is
JavaScript, the host is Haskell, and the embeddable subset is typed.
Binders use PHOAS, so terms cannot reference unbound variables,
substitution is function application, and capture is impossible.

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.IO as T
import JShark.Prelude
import qualified JShark.Console as Console

greet :: Expr f 'String -> Effect f 'Unit
greet name = fromSyntax $ do
  Console.log ("hello, " <> name)
  done

main :: IO ()
main = compileEffect readableConfig (greet (string "world")) >>= T.putStrLn
```

The pure side of the language can be evaluated in GHCi with no JavaScript
engine:

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
