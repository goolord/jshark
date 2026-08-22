{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | JS @String.prototype@ wrappers. Opaque to 'JShark.evaluate'.
-- Built on closed-name 'ExprUnary' / 'ExprBinary' / 'ExprTernary' nodes.
-- Import qualified; names clash with 'Prelude'.
module JShark.String
  ( length
  , indexOf
  , slice
  , toUpper
  , toLower
  , trim
  , split
  , replace
  )
where

import JShark.Types
import Prelude hiding (length)

-- | @s.length@
length :: Expr f 'String -> Expr f 'Number
length = ExprUnary StdStrLen

-- | @s.indexOf(sub)@
indexOf :: Expr f 'String -> Expr f 'String -> Expr f 'Number
indexOf = ExprBinary StdIndexOf

-- | @s.slice(start, end)@
slice :: Expr f 'String -> Expr f 'Number -> Expr f 'Number -> Expr f 'String
slice = ExprTernary StdSlice

-- | @s.toUpperCase()@
toUpper :: Expr f 'String -> Expr f 'String
toUpper = ExprUnary StdToUpper

-- | @s.toLowerCase()@
toLower :: Expr f 'String -> Expr f 'String
toLower = ExprUnary StdToLower

-- | @s.trim()@
trim :: Expr f 'String -> Expr f 'String
trim = ExprUnary StdTrim

-- | @s.split(sep)@
split :: Expr f 'String -> Expr f 'String -> Expr f ('Array 'String)
split = ExprBinary StdSplit

-- | @s.replace(pat, rep)@
replace :: Expr f 'String -> Expr f 'String -> Expr f 'String -> Expr f 'String
replace = ExprTernary StdReplace
