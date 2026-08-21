{-# LANGUAGE
    DataKinds
  , OverloadedStrings
#-}
-- | JS @String.prototype@ wrappers. Opaque to 'JShark.evaluate'.
module JShark.String
  ( length_
  , indexOf
  , slice
  , toUpper
  , toLower
  , trim
  , split
  , replace
  ) where

import JShark.Api
import JShark.Rec ((<:), Rec(..))
import JShark.Types

-- | @s.length@
length_ :: Expr f 'String -> Expr f 'Number
length_ s = exprProp s "length"

-- | @s.indexOf(sub)@
indexOf :: Expr f 'String -> Expr f 'String -> Expr f 'Number
indexOf s sub = exprMethod s "indexOf" (sub <: RecNil)

-- | @s.slice(start, end)@
slice :: Expr f 'String -> Expr f 'Number -> Expr f 'Number -> Expr f 'String
slice s start end = exprMethod s "slice" (start <: end <: RecNil)

-- | @s.toUpperCase()@
toUpper :: Expr f 'String -> Expr f 'String
toUpper s = exprMethod s "toUpperCase" RecNil

-- | @s.toLowerCase()@
toLower :: Expr f 'String -> Expr f 'String
toLower s = exprMethod s "toLowerCase" RecNil

-- | @s.trim()@
trim :: Expr f 'String -> Expr f 'String
trim s = exprMethod s "trim" RecNil

-- | @s.split(sep)@
split :: Expr f 'String -> Expr f 'String -> Expr f ('Array 'String)
split s sep = exprMethod s "split" (sep <: RecNil)

-- | @s.replace(pat, rep)@
replace :: Expr f 'String -> Expr f 'String -> Expr f 'String -> Expr f 'String
replace s pat rep = exprMethod s "replace" (pat <: rep <: RecNil)
