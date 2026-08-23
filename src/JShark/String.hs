{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | JS @String.prototype@ wrappers. Opaque to 'JShark.evaluate'.
-- Built on 'Std' 'Un' / 'Bin' / 'Tern'. Import qualified; names clash
-- with 'Prelude'.
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
length = Std . Un StdStrLen

-- | @s.indexOf(sub)@
indexOf :: Expr f 'String -> Expr f 'String -> Expr f 'Number
indexOf s sub = Std (Bin StdIndexOf s sub)

-- | @s.slice(start, end)@
slice :: Expr f 'String -> Expr f 'Number -> Expr f 'Number -> Expr f 'String
slice s a b = Std (Tern StdSlice s a b)

-- | @s.toUpperCase()@
toUpper :: Expr f 'String -> Expr f 'String
toUpper = Std . Un StdToUpper

-- | @s.toLowerCase()@
toLower :: Expr f 'String -> Expr f 'String
toLower = Std . Un StdToLower

-- | @s.trim()@
trim :: Expr f 'String -> Expr f 'String
trim = Std . Un StdTrim

-- | @s.split(sep)@
split :: Expr f 'String -> Expr f 'String -> Expr f ('Array 'String)
split s sep = Std (Bin StdSplit s sep)

-- | @s.replace(pat, rep)@
replace :: Expr f 'String -> Expr f 'String -> Expr f 'String -> Expr f 'String
replace s pat rep = Std (Tern StdReplace s pat rep)
