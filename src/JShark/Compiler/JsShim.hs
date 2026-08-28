{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Compiler-owned JS runtime shims and program-hoisted @$name@
-- bindings, collected during emit and printed once ahead of the body.
--
-- Shims are a closed catalog ('Builtin'). 'useShim' records a constructor
-- and returns the @$name(… )@ call. Named-lambda hoist ('insertHoisted')
-- is a separate bag on 'Preamble' — those bodies are program-specific.
-- 'renderPreamble' prints both, name-sorted, as @const $name = …;@ so
-- call-time order does not matter (function expressions close over the
-- bindings).
module JShark.Compiler.JsShim
  ( Builtin (..)
  , Preamble
  , emptyPreamble
  , useShim
  , insertHoisted
  , mergePreamble
  , renderPreamble
  , renderPreambleStyled
  , builtinSrc
  )
where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import JShark.Compiler.Emit
  ( JS
  , hcat
  , jsText
  , parens
  , punctuate
  , semi
  , vcat
  , (<+>)
  )
import JShark.Compiler.Hoist.Canonical (canonicalHoistSrc)

-- | Closed runtime shims. 'ValueEq' pulls the whole eq clique
-- ('ArrayEq', 'DeepEqual', 'Uint8ArrayEq') because those bodies call
-- each other.
data Builtin
  = CheckedIndex
  | ValueEq
  | ArrayEq
  | DeepEqual
  | Uint8ArrayEq
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | Runtime shims plus hoisted program lambdas, printed once ahead of
-- the body.
data Preamble = Preamble
  { pBuiltins :: !(S.Set Builtin)
  , pHoisted :: !(M.Map Text Text)
  }
  deriving Eq

emptyPreamble :: Preamble
emptyPreamble = Preamble S.empty M.empty

builtinNameSet :: S.Set Text
builtinNameSet = S.fromList (map builtinName [minBound .. maxBound])

needBuiltin :: Builtin -> Preamble -> Preamble
needBuiltin b p
  | b `S.member` eqClique =
      p {pBuiltins = pBuiltins p <> eqClique}
  | otherwise =
      p {pBuiltins = S.insert b (pBuiltins p)}

eqClique :: S.Set Builtin
eqClique = S.fromList [ValueEq, ArrayEq, DeepEqual, Uint8ArrayEq]

useShim :: Builtin -> [JS] -> Preamble -> (Preamble, JS)
useShim b args p = (needBuiltin b p, callShim b args)

insertHoisted :: Text -> Text -> Preamble -> Preamble
insertHoisted name src p
  | name `S.member` builtinNameSet =
      error $
        "JShark.insertHoisted: "
          <> T.unpack name
          <> " is a reserved shim name"
  | otherwise =
      p
        { pHoisted =
            M.insertWith
              (\incoming existing -> mergeHoistedSrc name existing incoming)
              name
              src
              (pHoisted p)
        }

mergePreamble :: Preamble -> Preamble -> Preamble
mergePreamble a b =
  assertDisjoint
    Preamble
      { pBuiltins = pBuiltins a <> pBuiltins b
      , pHoisted =
          M.unionWithKey
            mergeHoistedSrc
            (pHoisted a)
            (pHoisted b)
      }

assertDisjoint :: Preamble -> Preamble
assertDisjoint p =
  case [ builtinName b
       | b <- S.toList (pBuiltins p)
       , builtinName b `M.member` pHoisted p
       ] of
    (name : _) ->
      error $
        "JShark.mergePreamble: "
          <> T.unpack name
          <> " used as both shim and hoist"
    [] -> p

mergeHoistedSrc :: Text -> Text -> Text -> Text
mergeHoistedSrc name existing incoming
  | existing == incoming = existing
  | canonicalHoistSrc existing == canonicalHoistSrc incoming = existing
  | otherwise =
      error $
        "JShark.mergeHoistedSrc: conflicting body for "
          <> T.unpack name

-- | @const $name = <src>;@ for every used shim and hoisted lambda,
-- sorted by name (same order as a single map).
renderPreamble :: Preamble -> JS
renderPreamble = renderPreambleStyled False

-- | Render preamble bindings. The @sourceNames@ flag is reserved for codegen
-- ('esSourceNames'); shim bodies stay compact and Biome formats the full emit.
renderPreambleStyled :: Bool -> Preamble -> JS
renderPreambleStyled _sourceNames p =
  vcat
    [ ("const" <+> jsText name <+> "=" <+> jsText src) <> semi
    | (name, src) <- preambleToList p
    ]

preambleToList :: Preamble -> [(Text, Text)]
preambleToList p =
  M.toAscList (builtinMap <> pHoisted p)
 where
  builtinMap =
    M.fromList
      [ (builtinName b, builtinSrc b)
      | b <- S.toAscList (pBuiltins p)
      ]

callShim :: Builtin -> [JS] -> JS
callShim b args =
  jsText (builtinName b) <> parens (hcat (punctuate ", " args))

builtinName :: Builtin -> Text
builtinName = \case
  CheckedIndex -> "$checkedIndex"
  ValueEq -> "$valueEq"
  ArrayEq -> "$arrayEq"
  DeepEqual -> "$deepEqual"
  Uint8ArrayEq -> "$uint8ArrayEq"

builtinSrc :: Builtin -> Text
builtinSrc = \case
  CheckedIndex ->
    "function(a,i){var n=Math.trunc(i);if(!(n>=0&&n<a.length))throw new Error(\"jshark: index\");return a[n];}"
  ValueEq ->
    "function(a,b){if(a===b)return true;if(a===null||b===null||typeof a!==\"object\"||typeof b!==\"object\")return false;if(Array.isArray(a)&&Array.isArray(b))return $arrayEq(a,b);if(a instanceof Uint8Array&&b instanceof Uint8Array)return $uint8ArrayEq(a,b);if(a.constructor===Object&&b.constructor===Object)return $deepEqual(a,b);return false}"
  ArrayEq ->
    "function(a,b){if(a===b)return true;if(!Array.isArray(b))return false;if(a.length!==b.length)return false;for(var i=0;i<a.length;i++)if(!$valueEq(a[i],b[i]))return false;return true}"
  DeepEqual ->
    "function(a,b){if(a===b)return true;if(a instanceof Date&&b instanceof Date)return a.getTime()===b.getTime();if(a instanceof RegExp&&b instanceof RegExp)return a.toString()===b.toString();var ka=Object.keys(a),kb=Object.keys(b);if(ka.length!==kb.length)return false;for(var i=0;i<ka.length;i++){var k=ka[i];if(!Object.prototype.hasOwnProperty.call(b,k))return false;var v1=a[k],v2=b[k],o=v1&&v2&&typeof v1==='object'&&typeof v2==='object';if(o){if(Array.isArray(v1)){if(!$arrayEq(v1,v2))return false}else if(v1 instanceof Uint8Array){if(!$uint8ArrayEq(v1,v2))return false}else if(!$deepEqual(v1,v2))return false}else if(v1!==v2&&!(Number.isNaN(v1)&&Number.isNaN(v2)))return false}return true}"
  Uint8ArrayEq ->
    "function(a,b){if(a===b)return true;if(a.length!==b.length)return false;for(var i=0;i<a.length;i++)if(a[i]!==b[i])return false;return true}"
