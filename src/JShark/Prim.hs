{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Fixed-arity pure JS stdlib: host math, JS names, and codegen templates.
-- Interpreter / optimizer dispatch lives in 'JShark' ('evalFixed', …).
module JShark.Prim
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
  , MathUnary (..)
  , MathBinary (..)
  , matchMathUnary
  , matchMathBinary
  )
where

import Data.Text (Text)
import JShark.Types
import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as P

-- | Witness that a 'FixedOp' is a unary @Math@ op (refines kind indices).
data MathUnary (a :: Universe) (b :: Universe) (c :: Universe) (u :: Universe) where
  MathUnary ::
    FixedOp 'Number 'Unit 'Unit 'Number -> MathUnary 'Number 'Unit 'Unit 'Number

-- | Witness that a 'FixedOp' is a binary @Math@ op (refines kind indices).
data MathBinary (a :: Universe) (b :: Universe) (c :: Universe) (u :: Universe) where
  MathBinary ::
    FixedOp 'Number 'Number 'Unit 'Number
    -> MathBinary 'Number 'Number 'Unit 'Number

-- | JS @Math.*@ unary names. Keep in sync with 'matchMathUnary'.
lookupMathUnary ::
  FixedOp a b c u -> Maybe (FixedOp 'Number 'Unit 'Unit 'Number, Text)
lookupMathUnary = \case
  FixAbs -> Just (FixAbs, "abs")
  FixSign -> Just (FixSign, "sign")
  FixSin -> Just (FixSin, "sin")
  FixCos -> Just (FixCos, "cos")
  FixTan -> Just (FixTan, "tan")
  FixAsin -> Just (FixAsin, "asin")
  FixAcos -> Just (FixAcos, "acos")
  FixAtan -> Just (FixAtan, "atan")
  FixSinh -> Just (FixSinh, "sinh")
  FixCosh -> Just (FixCosh, "cosh")
  FixTanh -> Just (FixTanh, "tanh")
  FixAsinh -> Just (FixAsinh, "asinh")
  FixAcosh -> Just (FixAcosh, "acosh")
  FixAtanh -> Just (FixAtanh, "atanh")
  FixSqrt -> Just (FixSqrt, "sqrt")
  FixCbrt -> Just (FixCbrt, "cbrt")
  FixExp -> Just (FixExp, "exp")
  FixLog -> Just (FixLog, "log")
  FixLog2 -> Just (FixLog2, "log2")
  FixLog10 -> Just (FixLog10, "log10")
  FixFloor -> Just (FixFloor, "floor")
  FixCeil -> Just (FixCeil, "ceil")
  FixRound -> Just (FixRound, "round")
  FixTrunc -> Just (FixTrunc, "trunc")
  _ -> Nothing

-- | JS @Math.*@ binary names. Keep in sync with 'matchMathBinary'.
lookupMathBinary ::
  FixedOp a b c u -> Maybe (FixedOp 'Number 'Number 'Unit 'Number, Text)
lookupMathBinary = \case
  FixPow -> Just (FixPow, "pow")
  FixAtan2 -> Just (FixAtan2, "atan2")
  FixMax -> Just (FixMax, "max")
  FixMin -> Just (FixMin, "min")
  FixHypot -> Just (FixHypot, "hypot")
  _ -> Nothing

matchMathUnary :: FixedOp a b c u -> Maybe (MathUnary a b c u)
matchMathUnary = \case
  FixAbs -> Just (MathUnary FixAbs)
  FixSign -> Just (MathUnary FixSign)
  FixSin -> Just (MathUnary FixSin)
  FixCos -> Just (MathUnary FixCos)
  FixTan -> Just (MathUnary FixTan)
  FixAsin -> Just (MathUnary FixAsin)
  FixAcos -> Just (MathUnary FixAcos)
  FixAtan -> Just (MathUnary FixAtan)
  FixSinh -> Just (MathUnary FixSinh)
  FixCosh -> Just (MathUnary FixCosh)
  FixTanh -> Just (MathUnary FixTanh)
  FixAsinh -> Just (MathUnary FixAsinh)
  FixAcosh -> Just (MathUnary FixAcosh)
  FixAtanh -> Just (MathUnary FixAtanh)
  FixSqrt -> Just (MathUnary FixSqrt)
  FixCbrt -> Just (MathUnary FixCbrt)
  FixExp -> Just (MathUnary FixExp)
  FixLog -> Just (MathUnary FixLog)
  FixLog2 -> Just (MathUnary FixLog2)
  FixLog10 -> Just (MathUnary FixLog10)
  FixFloor -> Just (MathUnary FixFloor)
  FixCeil -> Just (MathUnary FixCeil)
  FixRound -> Just (MathUnary FixRound)
  FixTrunc -> Just (MathUnary FixTrunc)
  _ -> Nothing

matchMathBinary :: FixedOp a b c u -> Maybe (MathBinary a b c u)
matchMathBinary = \case
  FixPow -> Just (MathBinary FixPow)
  FixAtan2 -> Just (MathBinary FixAtan2)
  FixMax -> Just (MathBinary FixMax)
  FixMin -> Just (MathBinary FixMin)
  FixHypot -> Just (MathBinary FixHypot)
  _ -> Nothing

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

fixedUnaryJS :: FixedOp a b c u -> Doc -> Doc
fixedUnaryJS n r = case n of
  FixToUpper -> r <> ".toUpperCase()"
  FixToLower -> r <> ".toLowerCase()"
  FixTrim -> r <> ".trim()"
  FixArrLen -> dotLength
  FixU8Len -> dotLength
  FixStrLen -> dotLength
  FixStringify -> "JSON.stringify" <> P.parens r
  _ -> error "JShark.Prim.fixedUnaryJS: not a std unary op"
 where
  dotLength = r <> ".length"

fixedBinaryJS :: FixedOp a b c u -> Doc -> Doc -> Doc
fixedBinaryJS n r a = case n of
  FixIndexOf -> r <> ".indexOf" <> P.parens a
  FixSplit -> r <> ".split" <> P.parens a
  FixIncludes -> r <> ".includes" <> P.parens a
  FixConcat -> r <> ".concat" <> P.parens a
  FixJoin -> r <> ".join" <> P.parens a
  FixTest -> r <> ".test" <> P.parens a
  FixParseInt -> "parseInt" <> P.parens (r <> ", " <> a)
  _ -> error "JShark.Prim.fixedBinaryJS: not a std binary op"

fixedTernaryJS :: FixedOp a b c u -> Doc -> Doc -> Doc -> Doc
fixedTernaryJS n r a b = case n of
  FixSlice -> slice
  FixArrSlice -> slice
  FixReplace -> r <> ".replace" <> P.parens (a <> ", " <> b)
  _ -> error "JShark.Prim.fixedTernaryJS: not a std ternary op"
 where
  slice = r <> ".slice" <> P.parens (a <> ", " <> b)
