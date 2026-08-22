{-# LANGUAGE
    DataKinds
  , OverloadedStrings
#-}
-- | JS @Array.prototype@ wrappers. Opaque to 'JShark.evaluate' except 'index'.
-- Read wrappers are closed-name 'Expr' nodes. 'push' is a 'CallMethod' on
-- 'Effect'.
module JShark.Array
  ( index
  , length_
  , map_
  , mapE
  , mapE_
  , filter_
  , filterE
  , filterE_
  , includes
  , concat_
  , join
  , push
  , push_
  , reduce_
  , arraySlice
  , sort_
  ) where

import JShark.Api
import JShark.Rec ((<:), Rec(..))
import JShark.Types

-- | @arr[i]@
index :: Expr f ('Array u) -> Expr f 'Number -> Expr f u
index = ExprIndex

-- | @arr.length@
length_ :: Expr f ('Array u) -> Expr f 'Number
length_ = ExprUnary StdArrLen

-- | @arr.map(function(x){...})@. The callback stays on 'Expr'.
map_ :: Expr f ('Array u) -> (Expr f u -> Expr f v) -> Expr f ('Array v)
map_ arr f = ExprMap arr (\x -> f (var x))

-- | @arr.filter(function(x){...})@. The callback stays on 'Expr'.
filter_ :: Expr f ('Array u) -> (Expr f u -> Expr f 'Bool) -> Expr f ('Array u)
filter_ arr f = ExprFilter arr (\x -> f (var x))

-- | @arr.map@ with an 'Effect' callback (e.g. 'getProp'' inside).
mapE :: Expr f ('Array u) -> (Expr f u -> Effect f v) -> Effect f ('Array v)
mapE arr f = callMethod (expr arr) "map" (ArgEffect (LambdaE (\x -> f (var x))) <: RecNil)

-- | 'mapE' in 'EffectSyntax'.
mapE_ :: Expr f ('Array u) -> (Expr f u -> EffectSyntax f (f v)) -> EffectSyntax f (Expr f ('Array v))
mapE_ arr f = fmap Var $ toSyntax $ mapE arr (\x -> fromSyntax (f x))

-- | @arr.filter@ with an 'Effect' callback (e.g. 'getProp'' inside).
filterE :: Expr f ('Array u) -> (Expr f u -> Effect f 'Bool) -> Effect f ('Array u)
filterE arr f = callMethod (expr arr) "filter" (ArgEffect (LambdaE (\x -> f (var x))) <: RecNil)

-- | 'filterE' in 'EffectSyntax'.
filterE_ :: Expr f ('Array u) -> (Expr f u -> EffectSyntax f (f 'Bool)) -> EffectSyntax f (Expr f ('Array u))
filterE_ arr f = fmap Var $ toSyntax $ filterE arr (\x -> fromSyntax (f x))

-- | @arr.includes(x)@
includes :: Expr f ('Array u) -> Expr f u -> Expr f 'Bool
includes = ExprBinary StdIncludes

-- | @xs.concat(ys)@
concat_ :: Expr f ('Array u) -> Expr f ('Array u) -> Expr f ('Array u)
concat_ = ExprBinary StdConcat

-- | @arr.join(sep)@
join :: Expr f ('Array u) -> Expr f 'String -> Expr f 'String
join = ExprBinary StdJoin

-- | @arr.push(x)@. Mutates in place; a 'CallMethod' on 'Effect'.
push :: Expr f ('Array u) -> Expr f u -> Effect f 'Unit
push arr x = callMethod (expr arr) "push" (arg x <: RecNil)

-- | 'push' in 'EffectSyntax'.
push_ :: Expr f ('Array u) -> Expr f u -> EffectSyntax f (f 'Unit)
push_ arr x = toSyntax $ push arr x

-- | @arr.reduce(function(acc,x){…}, z)@
reduce_ :: Expr f ('Array u) -> Expr f v -> (Expr f v -> Expr f u -> Expr f v) -> Expr f v
reduce_ arr z f = ExprReduce arr z (\a x -> f (var a) (var x))

-- | @arr.slice(start, end)@. Copy; does not mutate.
arraySlice :: Expr f ('Array u) -> Expr f 'Number -> Expr f 'Number -> Expr f ('Array u)
arraySlice = ExprTernary StdArrSlice

-- | @arr.sort(function(a,b){…})@. Mutates in place. The compare callback
-- is a binary JS function (not curried 'lambda2'); it should return a
-- 'Number' (negative / zero / positive).
sort_ :: Expr f ('Array u) -> (Expr f u -> Expr f u -> Expr f 'Number) -> Effect f ('Array u)
sort_ arr cmp = ArraySort arr (\a b -> cmp (var a) (var b))
