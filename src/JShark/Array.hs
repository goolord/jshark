{-# LANGUAGE
    DataKinds
  , OverloadedStrings
#-}
-- | JS @Array.prototype@ wrappers. Opaque to 'JShark.evaluate' except 'index'.
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
length_ arr = exprProp arr "length"

-- | @arr.map(function(x){...})@
map_ :: Expr f ('Array u) -> (Expr f u -> Expr f v) -> Expr f ('Array v)
map_ arr f = exprMethodCallback arr "map" f

-- | @arr.filter(function(x){...})@
filter_ :: Expr f ('Array u) -> (Expr f u -> Expr f 'Bool) -> Expr f ('Array u)
filter_ arr f = exprMethodCallback arr "filter" f

-- | @arr.includes(x)@
includes :: Expr f ('Array u) -> Expr f u -> Expr f 'Bool
includes arr x = exprMethod arr "includes" (x <: RecNil)

-- | @xs.concat(ys)@
concat_ :: Expr f ('Array u) -> Expr f ('Array u) -> Expr f ('Array u)
concat_ xs ys = exprMethod xs "concat" (ys <: RecNil)

-- | @arr.join(sep)@
join :: Expr f ('Array u) -> Expr f 'String -> Expr f 'String
join arr sep = exprMethod arr "join" (sep <: RecNil)

-- | @arr.push(x)@. Mutates in place; lives in 'Effect' so it sequences
-- correctly with other effects.
push :: Expr f ('Array u) -> Expr f u -> Effect f 'Unit
push arr x = expr (exprMethod arr "push" (x <: RecNil))

-- | 'push' in 'EffectSyntax'.
push_ :: Expr f ('Array u) -> Expr f u -> EffectSyntax f (f 'Unit)
push_ arr x = toSyntax $ push arr x
