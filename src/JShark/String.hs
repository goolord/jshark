{-# LANGUAGE
    DataKinds
  , OverloadedStrings
#-}
-- | JS @String.prototype@ wrappers. Opaque to 'JShark.evaluate'.
-- Built on 'UnsafeExpr*'; assumed observationally pure.
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
length_ s = unsafeExprProp s "length"

-- | @s.indexOf(sub)@
indexOf :: Expr f 'String -> Expr f 'String -> Expr f 'Number
indexOf s sub = unsafeExprMethod s "indexOf" (sub <: RecNil)

-- | @s.slice(start, end)@
slice :: Expr f 'String -> Expr f 'Number -> Expr f 'Number -> Expr f 'String
slice s start end = unsafeExprMethod s "slice" (start <: end <: RecNil)

-- | @s.toUpperCase()@
toUpper :: Expr f 'String -> Expr f 'String
toUpper s = unsafeExprMethod s "toUpperCase" RecNil

-- | @s.toLowerCase()@
toLower :: Expr f 'String -> Expr f 'String
toLower s = unsafeExprMethod s "toLowerCase" RecNil

-- | @s.trim()@
trim :: Expr f 'String -> Expr f 'String
trim s = unsafeExprMethod s "trim" RecNil

-- | @s.split(sep)@
split :: Expr f 'String -> Expr f 'String -> Expr f ('Array 'String)
split s sep = unsafeExprMethod s "split" (sep <: RecNil)

-- | @s.replace(pat, rep)@
replace :: Expr f 'String -> Expr f 'String -> Expr f 'String -> Expr f 'String
replace s pat rep = unsafeExprMethod s "replace" (pat <: rep <: RecNil)
