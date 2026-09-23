# jshark-base

Everything around the [`jshark`](https://github.com/goolord/jshark/tree/master/packages/jshark)
core that is not the AST or the compiler:

- typed bindings for the JavaScript standard library and browser platform:
  `JShark.Array`, `JShark.Map`, `JShark.Set`, `JShark.String`, `JShark.Math`,
  `JShark.Json`, `JShark.Regex`, `JShark.Promise`, `JShark.Ajax`,
  `JShark.Console`, `JShark.Dom` (including events), `JShark.Canvas`,
  `JShark.Storage`, `JShark.Timers`, `JShark.Worker`;
- `JShark.Generic`: `Generic` records and sums as JS objects;
- `JShark.Classes`: universe-indexed `Functor` \/ `Monad` \/ `Foldable` …;
- `JShark.Build`: the IO compile driver (Biome formatting, batch compiles,
  CLI flags);
- `JShark.Bun`: run compiled programs under Bun (optionally with happy-dom);
- `JShark.Prelude`: one import for a typical program.

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
