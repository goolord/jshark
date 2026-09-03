{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-gadt-mono-local-binds #-}

-- | Compile-time constant folding for PHOAS optimize.
module JShark.Compiler.Optimize.Fold
  ( foldNum1
  , foldNum2
  , foldConcat
  , foldAnd
  , foldOr
  , foldCmp
  , foldFrozenEq
  , peelFrozen
  , foldOrd
  , foldOrdNeq
  , foldShow
  , foldTypeOf
  , foldIndex
  , foldFixedUnary
  , foldFixedBinary
  , foldArrLen
  , foldToBigInt
  , foldFromBigInt
  , foldParseBigInt
  , foldBig
  , foldBigNeg
  )
where

import qualified Data.Text as T
import qualified JShark.Api.Prim as Prim
  ( exactMathBinary
  , exactMathUnary
  , isFiniteDouble
  )
import JShark.Api.Types
import JShark.Compiler.Binder (Stamp (..))
import JShark.Compiler.Evaluate
  ( eqFoldableValue
  , isOrderableValue
  , jsShow
  , parseBigIntString
  , tryEvalBigBin
  , typeOfValue
  , valueCompare
  )
import JShark.Compiler.Optimize.Analysis (pureExpr)

foldNum1 ::
  (Double -> Double)
  -> (Expr Stamp 'Number -> Expr Stamp 'Number)
  -> Expr Stamp 'Number
  -> Expr Stamp 'Number
foldNum1 f k = \case
  Literal (ValueNumber a) -> Literal (ValueNumber (f a))
  x -> k x

foldNum2 ::
  (Double -> Double -> Double)
  -> (Expr Stamp 'Number -> Expr Stamp 'Number -> Expr Stamp 'Number)
  -> Expr Stamp 'Number
  -> Expr Stamp 'Number
  -> Expr Stamp 'Number
foldNum2 f k x y = case (x, y) of
  (Literal (ValueNumber a), Literal (ValueNumber b)) -> Literal (ValueNumber (f a b))
  _ -> k x y

foldConcat :: Expr Stamp 'String -> Expr Stamp 'String -> Expr Stamp 'String
foldConcat x y = case (x, y) of
  (Literal (ValueString a), Literal (ValueString b)) -> Literal (ValueString (a <> b))
  _ -> Concat x y

foldAnd :: Expr Stamp 'Bool -> Expr Stamp 'Bool -> Expr Stamp 'Bool
foldAnd x y = case (x, y) of
  (Literal (ValueBool False), _) -> Literal (ValueBool False)
  (Literal (ValueBool True), y') -> y'
  (_, Literal (ValueBool True)) -> x
  (x', Literal (ValueBool False)) | pureExpr x' -> Literal (ValueBool False)
  _ -> Std (Kernel (KAnd x y))

foldOr :: Expr Stamp 'Bool -> Expr Stamp 'Bool -> Expr Stamp 'Bool
foldOr x y = case (x, y) of
  (Literal (ValueBool True), _) -> Literal (ValueBool True)
  (Literal (ValueBool False), y') -> y'
  (_, Literal (ValueBool False)) -> x
  (x', Literal (ValueBool True)) | pureExpr x' -> Literal (ValueBool True)
  _ -> Std (Kernel (KOr x y))

foldCmp ::
  (Value u -> Value u -> Bool)
  -> (Value u -> Bool)
  -> (Expr Stamp u -> Expr Stamp u -> Expr Stamp 'Bool)
  -> Expr Stamp u
  -> Expr Stamp u
  -> Expr Stamp 'Bool
foldCmp cmp ok k x y = case (x, y) of
  (Literal a, Literal b) | ok a && ok b -> Literal (ValueBool (cmp a b))
  _ -> k x y

foldFrozenEq ::
  (forall a. Value a -> Value a -> Bool)
  -> (Expr Stamp u -> Expr Stamp u -> Expr Stamp 'Bool)
  -> Expr Stamp u
  -> Expr Stamp u
  -> Expr Stamp 'Bool
foldFrozenEq cmp k x y = case (x, y) of
  (Literal a, Literal b)
    | eqFoldableValue a && eqFoldableValue b ->
        Literal (ValueBool (cmp a b))
  (FrozenLit as, FrozenLit bs)
    | Just as' <- peelFrozen as
    , Just bs' <- peelFrozen bs ->
        Literal (ValueBool (cmp (ValueFrozen as') (ValueFrozen bs')))
  _ -> k x y

peelFrozen :: [FieldLit Stamp r] -> Maybe [FieldLit Value r]
peelFrozen = traverse $ \case
  FieldLit @k e -> case e of
    Literal v -> Just (FieldLit @k (Literal v))
    _ -> Nothing
  FieldLitExtra @k e -> case e of
    Literal v -> Just (FieldLitExtra @k (Literal v))
    _ -> Nothing
  FieldLitEffect {} -> Nothing
  FieldLitExtraEffect {} -> Nothing

foldOrd ::
  Ordering
  -> (Expr Stamp u -> Expr Stamp u -> Expr Stamp 'Bool)
  -> Expr Stamp u
  -> Expr Stamp u
  -> Expr Stamp 'Bool
foldOrd ord = foldCmp (\a b -> valueCompare a b == ord) isOrderableValue

foldOrdNeq ::
  Ordering
  -> (Expr Stamp u -> Expr Stamp u -> Expr Stamp 'Bool)
  -> Expr Stamp u
  -> Expr Stamp u
  -> Expr Stamp 'Bool
foldOrdNeq ord = foldCmp (\a b -> valueCompare a b /= ord) isOrderableValue

foldShow :: Expr Stamp u -> Expr Stamp 'String
foldShow x = case x of
  Literal (ValueFunction _) -> Show x
  Literal v -> Literal (ValueString (jsShow v))
  _ -> Show x

foldTypeOf :: Expr Stamp u -> Expr Stamp 'String
foldTypeOf x = case x of
  Literal v -> Literal (ValueString (typeOfValue v))
  _ -> TypeOf x

foldIndex :: Expr Stamp ('Array u) -> Expr Stamp 'Number -> Expr Stamp u
foldIndex arr idx = case (arr, idx) of
  (Index {}, _) -> Index arr idx
  (Literal (ValueArray vs), Literal (ValueNumber d))
    | Prim.isFiniteDouble d
    , let
        i = truncate d :: Int
    , i >= 0 && i < length vs ->
        Literal (vs !! i)
  _ -> Index arr idx

foldFixedUnary ::
  FixedOp Number 'Unit 'Unit Number -> Expr Stamp 'Number -> Expr Stamp 'Number
foldFixedUnary n x = case x of
  Literal (ValueNumber a)
    | Just r <- Prim.exactMathUnary n a -> Literal (ValueNumber r)
  _ -> expr1 n x

foldFixedBinary ::
  FixedOp 'Number 'Number 'Unit 'Number
  -> Expr Stamp 'Number
  -> Expr Stamp 'Number
  -> Expr Stamp 'Number
foldFixedBinary n x y = case (x, y) of
  (Literal (ValueNumber a), Literal (ValueNumber b))
    | Just r <- Prim.exactMathBinary n a b -> Literal (ValueNumber r)
  _ -> expr2 n x y

foldArrLen :: Expr Stamp ('Array u) -> Expr Stamp 'Number
foldArrLen x = case x of
  Literal (ValueArray vs) ->
    Literal (ValueNumber (fromIntegral (Prelude.length vs)))
  _ -> expr1 FixArrLen x

foldToBigInt :: Expr Stamp 'Number -> Expr Stamp 'BigInt
foldToBigInt x = case x of
  Literal (ValueNumber d)
    | Prim.isFiniteDouble d
    , let
        n = truncate d
    , d == fromInteger n ->
        Literal (ValueBigInt n)
  _ -> expr1 FixToBigInt x

foldFromBigInt :: Expr Stamp 'BigInt -> Expr Stamp 'Number
foldFromBigInt x = case x of
  Literal (ValueBigInt n) -> Literal (ValueNumber (fromInteger n))
  _ -> expr1 FixFromBigInt x

foldParseBigInt :: Expr Stamp 'String -> Expr Stamp 'BigInt
foldParseBigInt x = case x of
  Literal (ValueString s)
    | Just n <- parseBigIntString (T.unpack s) ->
        Literal (ValueBigInt n)
  _ -> expr1 FixParseBigInt x

foldBig ::
  BigBinOp
  -> Expr Stamp 'BigInt
  -> Expr Stamp 'BigInt
  -> Expr Stamp 'BigInt
foldBig op x y = case (x, y) of
  (Literal (ValueBigInt a), Literal (ValueBigInt b))
    | Just r <- tryEvalBigBin op a b ->
        Literal (ValueBigInt r)
  _ -> Std (Kernel (KBig op x y))

foldBigNeg :: Expr Stamp 'BigInt -> Expr Stamp 'BigInt
foldBigNeg x = case x of
  Literal (ValueBigInt n) -> Literal (ValueBigInt (negate n))
  _ -> Std (Kernel (KBigNeg x))
