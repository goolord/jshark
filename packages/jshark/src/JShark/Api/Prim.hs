{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Fixed-arity pure JS stdlib: host math, JS names, and codegen templates.
-- Interpreter / optimizer dispatch lives in Evaluate / Optimize.
module JShark.Api.Prim
  ( mathUnaryFn
  , mathBinaryFn
  , exactMathUnary
  , exactMathBinary
  , math1Name
  , math2Name
  , fixedUnaryJS
  , fixedBinaryJS
  , fixedTernaryJS
  , isPureFixed
  , isFiniteDouble
  , MathUnary (..)
  , MathBinary (..)
  , matchMathUnary
  , matchMathBinary
  )
where

import Data.Text (Text)
import JShark.Api.Types
import JShark.Compiler.Emit (JS, parens)

-- | Witness that a 'FixedOp' is a unary @Math@ op (refines kind indices).
data MathUnary (a :: Universe) (b :: Universe) (c :: Universe) (u :: Universe) where
  MathUnary ::
    FixedOp 'Number 'Unit 'Unit 'Number -> MathUnary 'Number 'Unit 'Unit 'Number

-- | Witness that a 'FixedOp' is a binary @Math@ op (refines kind indices).
data MathBinary (a :: Universe) (b :: Universe) (c :: Universe) (u :: Universe) where
  MathBinary ::
    FixedOp 'Number 'Number 'Unit 'Number
    -> MathBinary 'Number 'Number 'Unit 'Number

-- | JS @Math.*@ unary ops: index witness + JS name.
lookupMathUnary :: FixedOp a b c u -> Maybe (MathUnary a b c u, Text)
lookupMathUnary = \case
  FixAbs -> Just (MathUnary FixAbs, "abs")
  FixSign -> Just (MathUnary FixSign, "sign")
  FixSin -> Just (MathUnary FixSin, "sin")
  FixCos -> Just (MathUnary FixCos, "cos")
  FixTan -> Just (MathUnary FixTan, "tan")
  FixAsin -> Just (MathUnary FixAsin, "asin")
  FixAcos -> Just (MathUnary FixAcos, "acos")
  FixAtan -> Just (MathUnary FixAtan, "atan")
  FixSinh -> Just (MathUnary FixSinh, "sinh")
  FixCosh -> Just (MathUnary FixCosh, "cosh")
  FixTanh -> Just (MathUnary FixTanh, "tanh")
  FixAsinh -> Just (MathUnary FixAsinh, "asinh")
  FixAcosh -> Just (MathUnary FixAcosh, "acosh")
  FixAtanh -> Just (MathUnary FixAtanh, "atanh")
  FixSqrt -> Just (MathUnary FixSqrt, "sqrt")
  FixCbrt -> Just (MathUnary FixCbrt, "cbrt")
  FixExp -> Just (MathUnary FixExp, "exp")
  FixLog -> Just (MathUnary FixLog, "log")
  FixLog2 -> Just (MathUnary FixLog2, "log2")
  FixLog10 -> Just (MathUnary FixLog10, "log10")
  FixFloor -> Just (MathUnary FixFloor, "floor")
  FixCeil -> Just (MathUnary FixCeil, "ceil")
  FixRound -> Just (MathUnary FixRound, "round")
  FixTrunc -> Just (MathUnary FixTrunc, "trunc")
  _ -> Nothing

-- | JS @Math.*@ binary ops: index witness + JS name.
lookupMathBinary :: FixedOp a b c u -> Maybe (MathBinary a b c u, Text)
lookupMathBinary = \case
  FixPow -> Just (MathBinary FixPow, "pow")
  FixAtan2 -> Just (MathBinary FixAtan2, "atan2")
  FixMax -> Just (MathBinary FixMax, "max")
  FixMin -> Just (MathBinary FixMin, "min")
  FixHypot -> Just (MathBinary FixHypot, "hypot")
  _ -> Nothing

matchMathUnary :: FixedOp a b c u -> Maybe (MathUnary a b c u)
matchMathUnary = fmap fst . lookupMathUnary

matchMathBinary :: FixedOp a b c u -> Maybe (MathBinary a b c u)
matchMathBinary = fmap fst . lookupMathBinary

math1Name :: FixedOp a b c u -> Maybe Text
math1Name op = fmap snd (lookupMathUnary op)

math2Name :: FixedOp a b c u -> Maybe Text
math2Name op = fmap snd (lookupMathBinary op)

mathUnaryFn :: FixedOp Number 'Unit 'Unit Number -> Double -> Double
mathUnaryFn = \case
  FixAbs -> abs
  FixSign -> signum
  FixSin -> sin
  FixCos -> cos
  FixTan -> tan
  FixAsin -> asin
  FixAcos -> acos
  FixAtan -> atan
  FixSinh -> sinh
  FixCosh -> cosh
  FixTanh -> tanh
  FixAsinh -> asinh
  FixAcosh -> acosh
  FixAtanh -> atanh
  FixSqrt -> sqrt
  FixCbrt -> \x -> signum x * (abs x ** (1 / 3))
  FixExp -> exp
  FixLog -> log
  FixLog2 -> logBase 2
  FixLog10 -> logBase 10
  FixFloor -> jsToIntegral floor
  FixCeil -> jsToIntegral ceiling
  -- JS's Math.round rounds half-way values toward +Infinity (e.g.
  -- Math.round(2.5) === 3, Math.round(-2.5) === -2), unlike Haskell's
  -- 'round' (banker's rounding to even: round 2.5 == 2). floor(x + 0.5)
  -- matches JS's semantics. Non-finite inputs are the identity.
  FixRound -> jsToIntegral (floor . (+ 0.5))
  FixTrunc -> jsToIntegral truncate

mathBinaryFn :: FixedOp Number Number 'Unit Number -> Double -> Double -> Double
mathBinaryFn = \case
  FixPow -> (**)
  FixAtan2 -> atan2
  FixMax -> max
  FixMin -> min
  FixHypot -> \x y -> sqrt (x * x + y * y)

exactMathUnary :: FixedOp Number 'Unit 'Unit Number -> Double -> Maybe Double
exactMathUnary n a = case n of
  FixAbs -> Just (abs a)
  FixSign | isFiniteDouble a -> Just (signum a)
  FixSin | a == 0 -> Just 0
  FixCos | a == 0 -> Just 1
  FixTan | a == 0 -> Just 0
  FixSinh | a == 0 -> Just 0
  FixCosh | a == 0 -> Just 1
  FixTanh | a == 0 -> Just 0
  FixAsinh | a == 0 -> Just 0
  FixAcosh | a == 1 -> Just 0
  FixAtanh | a == 0 -> Just 0
  FixSqrt
    | a >= 0
    , let
        r = sqrt a
    , r * r == a ->
        Just r
  FixFloor | isFiniteDouble a -> Just (fromIntegral (floor a :: Integer))
  FixCeil | isFiniteDouble a -> Just (fromIntegral (ceiling a :: Integer))
  FixRound | isFiniteDouble a -> Just (fromIntegral (floor (a + 0.5) :: Integer))
  FixTrunc | isFiniteDouble a -> Just (fromIntegral (truncate a :: Integer))
  _ -> Nothing

exactMathBinary ::
  FixedOp Number Number 'Unit Number -> Double -> Double -> Maybe Double
exactMathBinary n a b = case n of
  FixMax | isFiniteDouble a && isFiniteDouble b -> Just (max a b)
  FixMin | isFiniteDouble a && isFiniteDouble b -> Just (min a b)
  _ -> Nothing

isFiniteDouble :: Double -> Bool
isFiniteDouble d = not (isNaN d) && not (isInfinite d)

jsToIntegral :: (Double -> Integer) -> Double -> Double
jsToIntegral f d
  | isFiniteDouble d = fromIntegral (f d)
  | otherwise = d

isPureFixed :: FixedOp a b c u -> Bool
isPureFixed FixStringify = False
isPureFixed _ = True

fixedUnaryJS :: FixedOp a b c u -> JS -> JS
fixedUnaryJS n r = case n of
  FixToUpper -> r <> ".toUpperCase()"
  FixToLower -> r <> ".toLowerCase()"
  FixTrim -> r <> ".trim()"
  FixArrLen -> dotLength
  FixU8Len -> dotLength
  FixStrLen -> dotLength
  FixStringify -> "JSON.stringify" <> parens r
  FixToBigInt -> "BigInt" <> parens r
  FixFromBigInt -> "Number" <> parens r
  FixParseBigInt -> "BigInt" <> parens r
  _ -> error "JShark.Api.Prim.fixedUnaryJS: not a std unary op"
 where
  dotLength = r <> ".length"

fixedBinaryJS :: FixedOp a b c u -> JS -> JS -> JS
fixedBinaryJS n r a = case n of
  FixIndexOf -> r <> ".indexOf" <> parens a
  FixSplit -> r <> ".split" <> parens a
  FixIncludes -> r <> ".includes" <> parens a
  FixConcat -> r <> ".concat" <> parens a
  FixJoin -> r <> ".join" <> parens a
  FixTest -> r <> ".test" <> parens a
  FixParseInt -> "parseInt" <> parens (r <> ", " <> a)
  _ -> error "JShark.Api.Prim.fixedBinaryJS: not a std binary op"

fixedTernaryJS :: FixedOp a b c u -> JS -> JS -> JS -> JS
fixedTernaryJS n r a b = case n of
  FixCall2 -> parens r <> parens (a <> ", " <> b)
  FixSlice -> slice
  FixArrSlice -> slice
  FixReplace -> r <> ".replace" <> parens (a <> ", " <> b)
  _ -> error "JShark.Api.Prim.fixedTernaryJS: not a std ternary op"
 where
  slice = r <> ".slice" <> parens (a <> ", " <> b)
