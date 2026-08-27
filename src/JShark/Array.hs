{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | JS @Array.prototype@ and small array algorithms.
--
-- Most reads compile to 'Std' / kernel 'Index'. Mutations ('push', 'clear',
-- 'sort') are 'Effect' / 'CallMethod'. Hoisted helpers (@$arrayIndex@,
-- @$groupBy@) come from 'namedLambdaRow' and are called with 'applyNamed2'.
--
-- Import qualified; names clash with 'Prelude'.
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
  , clear
  , clear_
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
  , toSorted
  )
where

import qualified Data.List as List
import JShark.Api
import JShark.Api.Params (Param)
import JShark.Api.Rec (Rec (..), (<:))
import JShark.Api.Types
import qualified JShark.Math as Math
import Prelude hiding (concat, filter, length, map, zipWith)

-- | @arr[i]@ after 'Math.trunc'. Out of range is 'Error'.
index :: Expr f ('Array u) -> Expr f 'Number -> Expr f u
index arr i =
  case foldArrayIndex arr i of
    Just e -> e
    Nothing -> applyNamed2 indexChecked arr i

foldArrayIndex ::
  Expr f ('Array u) -> Expr f 'Number -> Maybe (Expr f u)
foldArrayIndex arr i = case (arr, i) of
  (Literal (ValueArray vs), Literal (ValueNumber d))
    | finiteDouble d
    , let
        idx = truncate d :: Int
    , idx >= 0
    , idx < List.length vs ->
        Just (Literal (vs !! idx))
  _ -> Nothing

finiteDouble :: Double -> Bool
finiteDouble d = not (isNaN d) && not (isInfinite d)

-- | Hoisted @$arrayIndex@ helper (bounds-checked 'Index').
indexChecked ::
  forall f u.
  Expr f ('Function ('Array u) ('Function 'Number u))
indexChecked =
  namedLambdaRow
    @('[Param "arr" ('Array u), Param "i" 'Number])
    "arrayIndex"
    $ \p ->
      let_ (Math.trunc p.i) $ \n ->
        if_
          (And (GTEq n 0) (LTh n (length p.arr)))
          (Index p.arr n)
          (Error (Literal (ValueString "array index out of bounds")))

-- | @arr.length@
length :: Expr f ('Array u) -> Expr f 'Number
length = expr1 FixArrLen

-- | @arr.map(function(x){...})@. The callback stays on 'Expr'.
map :: Expr f ('Array u) -> (Expr f u -> Expr f v) -> Expr f ('Array v)
map arr f = Std (Method (MethMap arr (\x -> f (var x))))

-- | @arr.filter(function(x){...})@. The callback stays on 'Expr'.
filter :: Expr f ('Array u) -> (Expr f u -> Expr f 'Bool) -> Expr f ('Array u)
filter arr f = Std (Method (MethFilter arr (\x -> f (var x))))

-- | @arr.map@ with an 'Effect' callback (e.g. 'getProp'' inside).
mapE :: Expr f ('Array u) -> (Expr f u -> Effect f v) -> Effect f ('Array v)
mapE = arrayCallback "map"

-- | 'mapE' in 'EffectSyntax'.
mapE_ ::
  Expr f ('Array u)
  -> (Expr f u -> EffectSyntax f (f v))
  -> EffectSyntax f (Expr f ('Array v))
mapE_ arr f = bindExpr $ mapE arr (\x -> fromSyntax (f x))

-- | @arr.filter@ with an 'Effect' callback (e.g. 'getProp'' inside).
filterE ::
  Expr f ('Array u) -> (Expr f u -> Effect f 'Bool) -> Effect f ('Array u)
filterE = arrayCallback "filter"

-- | 'filterE' in 'EffectSyntax'.
filterE_ ::
  Expr f ('Array u)
  -> (Expr f u -> EffectSyntax f (f 'Bool))
  -> EffectSyntax f (Expr f ('Array u))
filterE_ arr f = bindExpr $ filterE arr (\x -> fromSyntax (f x))

-- | @arr.includes(x)@
includes :: Expr f ('Array u) -> Expr f u -> Expr f 'Bool
includes xs x = expr2 FixIncludes xs x

-- | @xs.concat(ys)@
concat :: Expr f ('Array u) -> Expr f ('Array u) -> Expr f ('Array u)
concat xs ys = expr2 FixConcat xs ys

-- | @arr.join(sep)@
join :: Expr f ('Array u) -> Expr f 'String -> Expr f 'String
join xs sep = expr2 FixJoin xs sep

-- | @arr.length = 0@. Clears in place without reallocating.
clear :: Expr f ('Array u) -> Effect f 'Unit
clear arr = ffi "a=>{a.length=0}" (arg arr <: RecNil)

-- | 'clear' in 'EffectSyntax'.
clear_ :: Expr f ('Array u) -> EffectSyntax f (f 'Unit)
clear_ arr = toSyntax $ clear arr

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
reduce arr z f = Std (Method (MethReduce arr z (\a x -> f (var a) (var x))))

-- | @arr.reduceRight(function(acc,x){…}, z)@. JS callback is still @(acc, x)@.
reduceRight ::
  Expr f ('Array u) -> Expr f v -> (Expr f v -> Expr f u -> Expr f v) -> Expr f v
reduceRight arr z f = Std (Method (MethReduceRight arr z (\a x -> f (var a) (var x))))

-- | @[x]@. One-element array; used by 'JShark.Api.Classes.pure'.
singleton :: Expr f u -> Expr f ('Array u)
singleton x = map (Literal (ValueArray [ValueUnit])) (\_ -> x)

-- | @[{key, items}]@ in first-seen key order. One 'reduce' pass; 'keyFn'
-- runs once per element.
groupBy ::
  Expr f ('Array u)
  -> (Expr f u -> Expr f 'String)
  -> Expr f ('Array ('Object (GroupBy u)))
groupBy arr keyFn = applyNamed2 groupByChecked arr (toLambda keyFn)

-- | Hoisted @$groupBy@ helper; one uncurried JS @function(arr, keyFn)@.
groupByChecked ::
  forall f u.
  Expr
    f
    ( 'Function
        ('Array u)
        ('Function ('Function u 'String) ('Array ('Object (GroupBy u))))
    )
groupByChecked =
  namedLambdaRow
    @('[Param "arr" ('Array u), Param "keyFn" ('Function u 'String)])
    "groupBy"
    $ \p ->
      reduce p.arr (Literal (ValueArray [])) $ \groups x ->
        let
          k = Apply p.keyFn x
         in
          let_
            ( reduce groups (Literal (ValueBool False)) $ \found g ->
                Or found (GetField @"key" g .== k)
            )
            $ \found ->
              if_
                found
                ( map groups $ \g ->
                    if_
                      (GetField @"key" g .== k)
                      (groupEntry k (concat (GetField @"items" g) (singleton x)))
                      g
                )
                (concat groups (singleton (groupEntry k (singleton x))))

-- | @{key, items}@ object used by 'groupBy'.
groupEntry ::
  Expr f 'String -> Expr f ('Array u) -> Expr f ('Object (GroupBy u))
groupEntry k items =
  FrozenLit [FieldLit @"key" k, FieldLit @"items" items]

-- | @zipWith@; result length is 'Math.min'. @Array.from@ over the indices.
zipWith ::
  (Expr f a -> Expr f b -> Expr f c)
  -> Expr f ('Array a)
  -> Expr f ('Array b)
  -> Expr f ('Array c)
zipWith f xs ys =
  let_ (Math.min (length xs) (length ys)) $ \n ->
    Std (Method (MethFrom n $ \i -> f (index xs (var i)) (index ys (var i))))

-- | @arr.slice(start, end)@. Copy; does not mutate.
arraySlice ::
  Expr f ('Array u) -> Expr f 'Number -> Expr f 'Number -> Expr f ('Array u)
arraySlice xs a b = expr3 FixArrSlice xs a b

-- | @arr.sort(function(a,b){…})@. Mutates in place.
sort ::
  Expr f ('Array u)
  -> (Expr f u -> Expr f u -> Expr f 'Number)
  -> Effect f ('Array u)
sort arr cmp =
  callMethod (expr arr) "sort" (arg (toFn cmp) <: RecNil)

-- | @arr.toSorted(function(a,b){…})@. Copy; does not mutate.
toSorted ::
  Expr f ('Array u)
  -> (Expr f u -> Expr f u -> Expr f 'Number)
  -> Expr f ('Array u)
toSorted arr cmp = Std (Method (MethToSorted arr (\a b -> cmp (var a) (var b))))
