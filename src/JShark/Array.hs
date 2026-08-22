{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

{- | JS @Array.prototype@ wrappers.
Read wrappers are closed-name 'Expr' nodes; 'evaluate' handles
'index', 'map', 'filter', 'reduce', 'reduceRight', 'groupBy', 'zipWith',
and 'singleton'.
'push' is a 'CallMethod' on 'Effect'. Import qualified; names clash
with 'Prelude'.
-}
module JShark.Array
  ( index
  , length
  , map
  , mapE
  , mapE_
  , filter
  , filterE
  , filterE_
  , includes
  , concat
  , join
  , push
  , push_
  , pushMany
  , pushMany_
  , fromEffects
  , reduce
  , reduceRight
  , singleton
  , groupBy
  , zipWith
  , arraySlice
  , sort
  )
where

import JShark.Api
import JShark.Rec (Rec (..), (<:))
import JShark.Types
import Prelude hiding (concat, filter, length, map, zipWith)

-- | @arr[i]@
index :: Expr f ('Array u) -> Expr f 'Number -> Expr f u
index = ExprIndex

-- | @arr.length@
length :: Expr f ('Array u) -> Expr f 'Number
length = ExprUnary StdArrLen

-- | @arr.map(function(x){...})@. The callback stays on 'Expr'.
map :: Expr f ('Array u) -> (Expr f u -> Expr f v) -> Expr f ('Array v)
map arr f = ExprMap arr (\x -> f (var x))

-- | @arr.filter(function(x){...})@. The callback stays on 'Expr'.
filter :: Expr f ('Array u) -> (Expr f u -> Expr f 'Bool) -> Expr f ('Array u)
filter arr f = ExprFilter arr (\x -> f (var x))

arrayMethod ::
  String -> Expr f ('Array u) -> (Expr f u -> Effect f v) -> Effect f w
arrayMethod name arr f =
  callMethod (expr arr) name (ArgEffect (LambdaE (\x -> f (var x))) <: RecNil)

-- | @arr.map@ with an 'Effect' callback (e.g. 'getProp'' inside).
mapE :: Expr f ('Array u) -> (Expr f u -> Effect f v) -> Effect f ('Array v)
mapE = arrayMethod "map"

-- | 'mapE' in 'EffectSyntax'.
mapE_ ::
  Expr f ('Array u)
  -> (Expr f u -> EffectSyntax f (f v))
  -> EffectSyntax f (Expr f ('Array v))
mapE_ arr f = fmap Var $ toSyntax $ mapE arr (\x -> fromSyntax (f x))

-- | @arr.filter@ with an 'Effect' callback (e.g. 'getProp'' inside).
filterE ::
  Expr f ('Array u) -> (Expr f u -> Effect f 'Bool) -> Effect f ('Array u)
filterE = arrayMethod "filter"

-- | 'filterE' in 'EffectSyntax'.
filterE_ ::
  Expr f ('Array u)
  -> (Expr f u -> EffectSyntax f (f 'Bool))
  -> EffectSyntax f (Expr f ('Array u))
filterE_ arr f = fmap Var $ toSyntax $ filterE arr (\x -> fromSyntax (f x))

-- | @arr.includes(x)@
includes :: Expr f ('Array u) -> Expr f u -> Expr f 'Bool
includes = ExprBinary StdIncludes

-- | @xs.concat(ys)@
concat :: Expr f ('Array u) -> Expr f ('Array u) -> Expr f ('Array u)
concat = ExprBinary StdConcat

-- | @arr.join(sep)@
join :: Expr f ('Array u) -> Expr f 'String -> Expr f 'String
join = ExprBinary StdJoin

-- | @arr.push(x)@. Mutates in place; a 'CallMethod' on 'Effect'.
push :: Expr f ('Array u) -> Expr f u -> Effect f 'Unit
push arr x = pushMany arr [x]

-- | 'push' in 'EffectSyntax'.
push_ :: Expr f ('Array u) -> Expr f u -> EffectSyntax f (f 'Unit)
push_ arr x = toSyntax $ push arr x

-- Pack a homogeneous argument list into the heterogeneous 'Rec' 'CallMethod' wants.
data SomeArgs f where
  SomeArgs :: Rec (Arg f) us -> SomeArgs f

-- | @arr.push(x, y, …)@. One call; mutates in place.
pushMany :: Expr f ('Array u) -> [Expr f u] -> Effect f 'Unit
pushMany arr xs =
  case foldr (\x (SomeArgs ys) -> SomeArgs (arg x <: ys)) (SomeArgs RecNil) xs of
    SomeArgs args -> callMethod (expr arr) "push" args

-- | 'pushMany' in 'EffectSyntax'.
pushMany_ :: Expr f ('Array u) -> [Expr f u] -> EffectSyntax f (f 'Unit)
pushMany_ arr xs = toSyntax $ pushMany arr xs

-- | @[e0, e1, …]@. Elements stay on 'Effect' (object / sum literals).
fromEffects :: [Effect f u] -> Effect f ('Array u)
fromEffects = ArrayLit

-- | @arr.reduce(function(acc,x){…}, z)@
reduce ::
  Expr f ('Array u) -> Expr f v -> (Expr f v -> Expr f u -> Expr f v) -> Expr f v
reduce arr z f = ExprReduce arr z (\a x -> f (var a) (var x))

-- | @arr.reduceRight(function(acc,x){…}, z)@. JS callback is still @(acc, x)@.
reduceRight ::
  Expr f ('Array u) -> Expr f v -> (Expr f v -> Expr f u -> Expr f v) -> Expr f v
reduceRight arr z f = ExprReduceRight arr z (\a x -> f (var a) (var x))

-- | @[x]@. One-element array; used by 'JShark.Classes.pure'.
singleton :: Expr f u -> Expr f ('Array u)
singleton x = ExprMap (Literal (ValueArray [ValueUnit])) (\_ -> x)

{- | @Object.groupBy@ as @[{key, items}]@. The key callback is unary
('String'); first-seen key order. Not a null-prototype object.
-}
groupBy ::
  Expr f ('Array u)
  -> (Expr f u -> Expr f 'String)
  -> Expr f ('Array ('Object (GroupBy u)))
groupBy arr f = ExprGroupBy arr (\x -> f (var x))

-- | @zipWith@; result length is @Math.min@.
zipWith ::
  (Expr f a -> Expr f b -> Expr f c)
  -> Expr f ('Array a)
  -> Expr f ('Array b)
  -> Expr f ('Array c)
zipWith f xs ys = ExprZipWith xs ys (\a b -> f (var a) (var b))

-- | @arr.slice(start, end)@. Copy; does not mutate.
arraySlice ::
  Expr f ('Array u) -> Expr f 'Number -> Expr f 'Number -> Expr f ('Array u)
arraySlice = ExprTernary StdArrSlice

{- | @arr.sort(function(a,b){…})@. Mutates in place. The compare callback
is a binary JS function (not curried 'lambda2'); it should return a
'Number' (negative / zero / positive).
-}
sort ::
  Expr f ('Array u)
  -> (Expr f u -> Expr f u -> Expr f 'Number)
  -> Effect f ('Array u)
sort arr cmp = ArraySort arr (\a b -> cmp (var a) (var b))
