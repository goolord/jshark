{-# LANGUAGE
    DataKinds
  , OverloadedStrings
#-}
-- | JS @Array.prototype@ wrappers. Opaque to 'JShark.evaluate' except 'index'.
-- Read wrappers return 'Expr' via 'UnsafeExpr*' (assumed observationally
-- pure). 'push' is a real 'CallMethod' on 'Effect'.
module JShark.Array
  ( index
  , length_
  , map_
  , filter_
  , includes
  , concat_
  , join
  , push
  , push_
  ) where

import JShark.Api
import JShark.Rec ((<:), Rec(..))
import JShark.Types

-- | @arr[i]@
index :: Expr f ('Array u) -> Expr f 'Number -> Expr f u
index = exprIndex

-- | @arr.length@
length_ :: Expr f ('Array u) -> Expr f 'Number
length_ arr = unsafeExprProp arr "length"

-- | @arr.map(function(x){...})@. The callback stays on 'Expr' and is
-- assumed observationally pure; an 'unsafeExprFfi' inside still type-checks
-- as a pure callback. Effectful work belongs on 'Effect' via 'callMethod'.
map_ :: Expr f ('Array u) -> (Expr f u -> Expr f v) -> Expr f ('Array v)
map_ arr f = unsafeExprMethodCallback arr "map" f

-- | @arr.filter(function(x){...})@. Same observational-purity assumption
-- as 'map_': the callback is 'Expr -> Expr', not an 'Effect'.
filter_ :: Expr f ('Array u) -> (Expr f u -> Expr f 'Bool) -> Expr f ('Array u)
filter_ arr f = unsafeExprMethodCallback arr "filter" f

-- | @arr.includes(x)@
includes :: Expr f ('Array u) -> Expr f u -> Expr f 'Bool
includes arr x = unsafeExprMethod arr "includes" (x <: RecNil)

-- | @xs.concat(ys)@
concat_ :: Expr f ('Array u) -> Expr f ('Array u) -> Expr f ('Array u)
concat_ xs ys = unsafeExprMethod xs "concat" (ys <: RecNil)

-- | @arr.join(sep)@
join :: Expr f ('Array u) -> Expr f 'String -> Expr f 'String
join arr sep = unsafeExprMethod arr "join" (sep <: RecNil)

-- | @arr.push(x)@. Mutates in place; a 'CallMethod' on 'Effect', not an
-- 'UnsafeExprMethod' smuggled through 'Lift'.
push :: Expr f ('Array u) -> Expr f u -> Effect f 'Unit
push arr x = callMethod (expr arr) "push" (arg x <: RecNil)

-- | 'push' in 'EffectSyntax'.
push_ :: Expr f ('Array u) -> Expr f u -> EffectSyntax f (f 'Unit)
push_ arr x = toSyntax $ push arr x
