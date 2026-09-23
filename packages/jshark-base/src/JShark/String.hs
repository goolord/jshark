{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | JS @String.prototype@ wrappers. Opaque to 'JShark.evaluate'.
-- Built on 'Std' 'Fixed'. Import qualified; names clash with 'Prelude'.
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

import JShark.Api.Types
import Prelude hiding (length)

-- | The number of UTF-16 code units (@s.length@).
length :: Expr f 'String -> Expr f 'Number
length = expr1 FixStrLen

-- | Index of the first occurrence of @sub@, or -1.
indexOf :: Expr f 'String -> Expr f 'String -> Expr f 'Number
indexOf s sub = expr2 FixIndexOf s sub

-- | @s.slice(a, b)@ — negative indices count from the end.
slice :: Expr f 'String -> Expr f 'Number -> Expr f 'Number -> Expr f 'String
slice s a b = expr3 FixSlice s a b

-- | @s.toUpperCase()@.
toUpper :: Expr f 'String -> Expr f 'String
toUpper = expr1 FixToUpper

-- | @s.toLowerCase()@.
toLower :: Expr f 'String -> Expr f 'String
toLower = expr1 FixToLower

-- | @s.trim()@.
trim :: Expr f 'String -> Expr f 'String
trim = expr1 FixTrim

-- | @s.split(sep)@ — an array of the pieces.
split :: Expr f 'String -> Expr f 'String -> Expr f ('Array 'String)
split s sep = expr2 FixSplit s sep

-- | @s.replace(from, to)@ — the FIRST occurrence only
-- (matches JS; no regex here).
replace :: Expr f 'String -> Expr f 'String -> Expr f 'String -> Expr f 'String
replace s pat rep = expr3 FixReplace s pat rep
