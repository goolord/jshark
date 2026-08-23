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

import JShark.Types
import Prelude hiding (length)

length :: Expr f 'String -> Expr f 'Number
length = expr1 FixStrLen

indexOf :: Expr f 'String -> Expr f 'String -> Expr f 'Number
indexOf s sub = expr2 FixIndexOf s sub

slice :: Expr f 'String -> Expr f 'Number -> Expr f 'Number -> Expr f 'String
slice s a b = expr3 FixSlice s a b

toUpper :: Expr f 'String -> Expr f 'String
toUpper = expr1 FixToUpper

toLower :: Expr f 'String -> Expr f 'String
toLower = expr1 FixToLower

trim :: Expr f 'String -> Expr f 'String
trim = expr1 FixTrim

split :: Expr f 'String -> Expr f 'String -> Expr f ('Array 'String)
split s sep = expr2 FixSplit s sep

replace :: Expr f 'String -> Expr f 'String -> Expr f 'String -> Expr f 'String
replace s pat rep = expr3 FixReplace s pat rep
