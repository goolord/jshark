{-# language DataKinds #-}
{-# language GADTs #-}

module JShark.Api where

import Topaz.Types
import Data.Text (Text)
import Data.Coerce (coerce)
import JShark.Types

host :: Effect f 'String
host = Host 

classAdd, classRemove, classToggle :: Expr f 'Element -> Expr f 'String -> Effect f 'Unit
classAdd = ClassAdd
classRemove = ClassRemove
classToggle = ClassToggle

lookupId ::
     Expr f 'String
  -> (Expr f 'Element -> Effect f u)
  -> Effect f u
lookupId x f = LookupId x (f . Var)

lookupSelector :: 
     Expr f 'String
  -> (Expr f ('Array 'Element) -> Effect f u)
  -> Effect f u
lookupSelector x f = LookupSelector x (f . Var)

consoleLog :: Expr f u -> Effect f 'Unit
consoleLog u = Log u

ffi :: String -> Rec (Expr f) us -> Effect f v
ffi name args = FFI name args

unsafeObject :: Expr f ('Object a) -> String -> Effect f u
unsafeObject = UnsafeObject

objectFfi :: Expr f ('Object a) -> Effect f b -> Effect f u
objectFfi = ObjectFFI

expr :: Expr f u -> Effect f u
expr = Lift

plus :: Expr f 'Number -> Expr f 'Number -> Expr f 'Number
plus a b = (Plus a b)

apply :: Expr f ('Function u v) -> Expr f u -> Expr f v
apply g a = (Apply g a)

let_ ::
     Expr f u
  -> (Expr f u -> Expr f v)
  -> Expr f v
let_ e f = (Let e (coerce f . Var))

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

