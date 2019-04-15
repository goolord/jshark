{-# language DataKinds #-}
{-# language GADTs #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}

module JShark.Api where

import Topaz.Types
import Data.Text (Text)
import Data.Coerce (coerce)
import JShark.Types
import JShark.Object
import Topaz.Rec ((<:))

data Window
type instance Field Window "location.host" = 'String
window :: Effect f ('Object Window)
window = undefined

host :: EffectSyntax f (Expr f 'String)
host = get @"location.host" window

onClick :: Effect f 'Element -> (f 'Unit -> Effect f a) -> EffectSyntax f ()
onClick el f = toSyntax_ $ unsafeObjectAssign (unsafeObject el "onClick") (LambdaE f)

consoleLog :: Expr f u -> EffectSyntax f ()
consoleLog u = toSyntax (ffi "console.log" (u <: RecNil)) *> pure ()

unEffectful :: Expr f ('Effectful u) -> Effect f u
unEffectful = UnEffectful

ffi :: String -> Rec (Expr f) us -> Effect f v
ffi name args = FFI name args

unsafeObject :: Effect f object -> String -> Effect f u
unsafeObject = UnsafeObject

unsafeObjectAssign :: Effect f object -> Effect f assignment -> Effect f u
unsafeObjectAssign = UnsafeObjectAssign

objectFfi :: Effect f object -> Effect f b -> Effect f u
objectFfi = ObjectFFI

expr :: Expr f u -> Effect f u
expr = Lift

plus :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
plus = Plus

apply :: Expr f ('Function u v) -> Expr f u -> Expr f v
apply = Apply

lambda :: 
     (Expr f u -> Expr f v)
  -> Expr f ('Function u v)
lambda f = Lambda (coerce f . Var)

number :: Double -> Expr f 'Number
number = Literal . ValueNumber

bool :: Bool -> Expr f 'Bool
bool = Literal . ValueBool

string :: Text -> Expr f 'String
string = Literal . ValueString

forEach :: Expr f ('Array u) -> (Expr f u -> Effect f u') -> Effect f 'Unit
forEach arr f = ForEach arr (coerce f . Var)

noOp :: Effect f 'Unit
noOp = expr (Literal ValueUnit)

let_ ::
     Expr f u
  -> (Expr f u -> Expr f v)
  -> Expr f v
let_ e f = (Let e (coerce f . Var)) 
