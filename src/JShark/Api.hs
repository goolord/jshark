{-# language DataKinds #-}
{-# language GADTs #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language OverloadedStrings #-}

module JShark.Api where

import Data.Text (Text)
import Data.Coerce (coerce)
import JShark.Types
import JShark.Object
import JShark.Rec (Rec(..), (<:))

data Window
type instance Field Window "location.host" = 'String

window :: Effect f ('Object Window)
window = unsafeObject "window"

host :: EffectSyntax f (Expr f 'String)
host = get @"location.host" window

onClick :: Effect f ('Object obj) -> (f 'Unit -> Effect f a) -> EffectSyntax f ()
onClick el f = toSyntax_ $ unsafeObjectAssign (unsafeObjectGet el "onclick") (LambdaE f)

consoleLog :: Expr f u -> EffectSyntax f ()
consoleLog u = toSyntax (ffi "console.log" (u <: RecNil)) *> pure ()

unEffectful :: Expr f ('Effectful u) -> Effect f u
unEffectful = UnEffectful

ffi :: String -> Rec (Expr f) us -> Effect f v
ffi name args = FFI name args

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

lambdaE :: 
    (Effect f u -> Effect f v) 
  -> Effect f ('Function u v)
lambdaE f = LambdaE (coerce f . Lift . Var)

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

-- Control flow -----------------------------------------------------------

-- | Ternary conditional: @if_ c t e@ compiles to @c ? t : e@.
if_ :: Expr f 'Bool -> Expr f u -> Expr f u -> Expr f u
if_ = If

-- | Effectful conditional. See the note on 'IfE' regarding the condition
-- expression's limitations.
ifE :: Expr f 'Bool -> Effect f u -> Effect f u -> Effect f u
ifE = IfE

-- | Run an effect only when the condition holds, otherwise do nothing.
when_ :: Expr f 'Bool -> Effect f 'Unit -> Effect f 'Unit
when_ c t = IfE c t noOp

-- | Loop while the condition holds. See the note on 'While' regarding the
-- condition expression's limitations.
while_ :: Expr f 'Bool -> Effect f 'Unit -> Effect f 'Unit
while_ = While

-- Option --------------------------------------------------------------

some :: Expr f u -> Expr f ('Option u)
some = Some

none :: Expr f ('Option u)
none = None

-- | Eliminate an 'Option', analogous to 'maybe'.
optionCase :: Expr f ('Option u) -> Expr f v -> (Expr f u -> Expr f v) -> Expr f v
optionCase opt noneBranch someBranch = OptionCase opt noneBranch (coerce someBranch . Var)

-- Result --------------------------------------------------------------

ok :: Expr f u -> Expr f ('Result u v)
ok = Ok

err :: Expr f v -> Expr f ('Result u v)
err = Err

-- | Eliminate a 'Result', analogous to 'either'.
resultCase :: Expr f ('Result u v) -> (Expr f u -> Expr f w) -> (Expr f v -> Expr f w) -> Expr f w
resultCase r okBranch errBranch = ResultCase r (coerce okBranch . Var) (coerce errBranch . Var)

unsafeEffectExpr :: Effect f u -> Expr f u
unsafeEffectExpr = UnsafeEffectExpr

exprFfi :: Text -> Rec (Expr f) us -> Expr f v
exprFfi = ExprFFI

exprProp :: Expr f u -> Text -> Expr f v
exprProp = ExprProp

exprMethod :: Expr f u -> Text -> Rec (Expr f) us -> Expr f v
exprMethod = ExprMethod

exprMethodCallback :: Expr f u -> Text -> (Expr f a -> Expr f b) -> Expr f v
exprMethodCallback recv name f = ExprMethodCallback recv name (coerce f . Var)

exprIndex :: Expr f ('Array u) -> Expr f 'Number -> Expr f u
exprIndex = ExprIndex

mathUnary :: Text -> Expr f 'Number -> Expr f 'Number
mathUnary = MathUnary

mathBinary :: Text -> Expr f 'Number -> Expr f 'Number -> Expr f 'Number
mathBinary = MathBinary

-- | Reinterpret a value that may be a JS null (e.g. the result of
-- @localStorage.getItem@) as an 'Option'. See the note on 'UnsafeNullable'.
unsafeNullable :: Expr f u -> Expr f ('Option u)
unsafeNullable = UnsafeNullable

addEventListener :: Text -> Effect f ('Object obj) -> (f u -> Effect f a) -> EffectSyntax f ()
addEventListener name el handler =
  toSyntax_ $ objectFfi el (ffi "addEventListener" (string name <: unsafeEffectExpr (LambdaE handler) <: RecNil))
