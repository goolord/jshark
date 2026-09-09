-- | One-import prelude for JShark programs.
--
-- Re-exports the EDSL surface ('JShark.Api'), the argument list syntax
-- (@arg@ \/ @(<:)@ \/ 'argEffect'), object literals ('JShark.Object'
-- constructors), and the compile driver ('JShark.Compiler'), so a typical
-- program needs this module plus qualified platform modules:
--
-- > {-# LANGUAGE DataKinds, OverloadedStrings #-}
-- > import JShark.Prelude
-- > import qualified JShark.Console as Console
-- >
-- > greet :: Expr f 'String -> Effect f 'Unit
-- > greet name = fromSyntax $ do
-- >   Console.log ("hello, " <> name)
-- >   done
-- >
-- > main :: IO ()
-- > main = compileEffectSyntax readableConfig (Console.log ("hello, " <> string "world")) >>= T.putStrLn
--
-- Platform bindings ('JShark.Dom', 'JShark.Array', 'JShark.Canvas', …)
-- stay qualified imports: they share many names with base and with each
-- other. 'JShark.Api.Classes' is deliberately not re-exported (it
-- shadows Prelude class methods); import it qualified when needed.
module JShark.Prelude
  ( module JShark.Api
  , module JShark.Api.Rec
  , module JShark.Compiler
  , module JShark.Object
  )
where

import JShark.Api
import JShark.Api.Rec (Rec (..), (<:))
import JShark.Compiler
import JShark.Object (field, frozen, obj)
