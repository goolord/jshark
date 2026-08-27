{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | Canonical shape hashes for catalog patterns. Used at runtime to
--    recognize emergent soup formations as known taxa before minting novel
--    species ids. Static bundles are emitted to @js/catalog.js@ (see
--    'catalogJs').
module Catalog
  ( shapeHash
  , canonicalShapeHash
  , catalogJs
  , nameWords
  , catalogKnown
  , catalogNames
  , catalogDisturb
  , catalogPrefixes
  , catalogSuffixes
  , catalogNouns
  , catalogAdjectives
  , catalogVerbsIng
  , catalogInitialCells
  , stampCatalogCells
  )
where

import Data.Char (ord)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import JShark.Api
import JShark.Api.Rec (Rec (..), (<:))
import JShark.Api.Types (Effect (FFI), FFIForm (FFILambda))
import Names (patternLabel)
import Numeric (showHex)
import Patterns
  ( PatternSpec (..)
  , allPatterns
  , disturbPatterns
  , initialCatalogCells
  )

normalizeCells :: [(Int, Int)] -> [(Int, Int)]
normalizeCells cells =
  let
    minX = minimum (map fst cells)
    minY = minimum (map snd cells)
   in
    sort [(x - minX, y - minY) | (x, y) <- cells]

shapeHash :: [(Int, Int)] -> Text
shapeHash cells =
  T.intercalate
    ";"
    [T.pack (show x ++ "," ++ show y) | (x, y) <- normalizeCells cells]

-- | Lexicographically smallest hash among the eight dihedral symmetries.
--   Rotations/reflections of the same still life or spaceship orientation
--   share one catalog key.
canonicalShapeHash :: [(Int, Int)] -> Text
canonicalShapeHash cells =
  minimum (map shapeHash (d4Normalized cells))

d4Normalized :: [(Int, Int)] -> [[(Int, Int)]]
d4Normalized cells =
  [normalizeCells [f (x, y) | (x, y) <- cells] | f <- d4Transforms]
 where
  d4Transforms :: [((Int, Int) -> (Int, Int))]
  d4Transforms =
    [ \(x, y) -> (x, y)
    , \(x, y) -> (-y, x)
    , \(x, y) -> (-x, -y)
    , \(x, y) -> (y, -x)
    , \(x, y) -> (-x, y)
    , \(x, y) -> (y, x)
    , \(x, y) -> (x, -y)
    , \(x, y) -> (-y, -x)
    ]

-- | One canonical entry per shape; duplicate orientations keep the lowest sid.
knownCatalog :: [(Text, Int)]
knownCatalog =
  foldr
    ( \p acc ->
        let
          hash = canonicalShapeHash (patCells p)
          sid = patId p
         in
          case lookup hash acc of
            Just old | old <= sid -> acc
            _ -> (hash, sid) : filter ((/= hash) . fst) acc
    )
    []
    allPatterns

-- | Word pools for procedural discovery names (also in 'catalogJs').
nameWords :: ([Text], [Text], [Text], [Text], [Text])
nameWords = (prefixes, suffixes, nouns, adjectives, verbsIng)

catalogJs :: Text
catalogJs =
  "globalThis.__lifeCatalog=()=>{const c=globalThis.LifeCatalog;if(!c){throw new Error('LifeCatalog missing: load js/catalog.js before app.js');}return c;};"
    <> "globalThis.LifeCatalog="
    <> knownJson
    <> ";\n"
 where
  knownJson =
    T.concat
      [ "{"
      , "\"known\":"
      , arrayJson (map knownEntry knownCatalog)
      , ",\"names\":"
      , arrayJson (map nameEntry allPatterns)
      , ",\"disturb\":"
      , arrayJson (map disturbEntry disturbPatterns)
      , ",\"initialCells\":"
      , initialCellsJson
      , ",\"words\":"
      , wordsJson
      , "}"
      ]
  knownEntry (hash, sid) =
    T.concat
      [ "[\""
      , hash
      , "\","
      , T.pack (show sid)
      , "]"
      ]
  nameEntry p =
    T.concat
      [ "["
      , T.pack (show (patId p))
      , ","
      , jsonString (patternLabel (patId p))
      , "]"
      ]
  disturbEntry p =
    T.concat
      [ "["
      , T.pack (show (patId p))
      , ","
      , cellsJson (patCells p)
      , "]"
      ]
  cellsJson cells =
    T.concat
      [ "["
      , T.intercalate
          ","
          [ "[" <> T.pack (show x) <> "," <> T.pack (show y) <> "]"
          | (x, y) <- cells
          ]
      , "]"
      ]
  wordsJson =
    T.concat
      [ "{"
      , "\"prefixes\":"
      , textArrayJson prefixes
      , ",\"suffixes\":"
      , textArrayJson suffixes
      , ",\"nouns\":"
      , textArrayJson nouns
      , ",\"adjectives\":"
      , textArrayJson adjectives
      , ",\"verbsIng\":"
      , textArrayJson verbsIng
      , "}"
      ]
  arrayJson items = "[" <> T.intercalate "," items <> "]"
  textArrayJson xs = arrayJson (map jsonString xs)
  initialCellsJson =
    arrayJson
      [ "[" <> T.pack (show i) <> "," <> T.pack (show w) <> "]"
      | (i, w) <- initialCatalogCells
      ]

catalogKnown
  , catalogNames
  , catalogDisturb
  , catalogInitialCells ::
    forall f u. Effect f u
catalogKnown = ffi "(()=>globalThis.__lifeCatalog().known)" RecNil
catalogNames = ffi "(()=>globalThis.__lifeCatalog().names)" RecNil
catalogDisturb = ffi "(()=>globalThis.__lifeCatalog().disturb)" RecNil
catalogInitialCells = ffi "(()=>globalThis.__lifeCatalog().initialCells)" RecNil

stampCatalogCells ::
  Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f ('Array ('Array 'Number))
  -> Effect f 'Unit
stampCatalogCells alive species cells =
  FFI
    ( FFILambda
        "(a,s,p)=>{for(let k=0;k<p.length;k++){const t=p[k];a[t[0]]=1;s[t[0]]=t[1];}}"
    )
    (arg alive <: arg species <: arg cells <: RecNil)

catalogPrefixes
  , catalogSuffixes
  , catalogNouns
  , catalogAdjectives
  , catalogVerbsIng ::
    forall f. Effect f ('Array 'String)
catalogPrefixes = ffi "(()=>globalThis.__lifeCatalog().words.prefixes)" RecNil
catalogSuffixes = ffi "(()=>globalThis.__lifeCatalog().words.suffixes)" RecNil
catalogNouns = ffi "(()=>globalThis.__lifeCatalog().words.nouns)" RecNil
catalogAdjectives = ffi "(()=>globalThis.__lifeCatalog().words.adjectives)" RecNil
catalogVerbsIng = ffi "(()=>globalThis.__lifeCatalog().words.verbsIng)" RecNil

jsonString :: Text -> Text
jsonString t =
  "\"" <> T.concat (map jsonChar (T.unpack t)) <> "\""
 where
  jsonChar '"' = "\\\""
  jsonChar '\\' = "\\\\"
  jsonChar '\n' = "\\n"
  jsonChar '\r' = "\\r"
  jsonChar '\t' = "\\t"
  jsonChar '\b' = "\\b"
  jsonChar '\f' = "\\f"
  jsonChar c
    | ord c == 0x2028 = "\\u2028"
    | ord c == 0x2029 = "\\u2029"
    | ord c < 0x20 = T.pack ("\\u" ++ jsonHex4 (ord c))
    | otherwise = T.singleton c

jsonHex4 :: Int -> String
jsonHex4 n =
  let
    h = showHex n ""
    pad = replicate (max 0 (4 - length h)) '0'
   in
    pad ++ h

prefixes, suffixes, nouns, adjectives, verbsIng :: [Text]
prefixes =
  [ "Nova"
  , "Mira"
  , "Axon"
  , "Zeph"
  , "Luma"
  , "Vex"
  , "Quin"
  , "Orb"
  , "Nex"
  , "Sol"
  , "Kael"
  , "Rune"
  , "Pyro"
  , "Cyan"
  , "Dusk"
  , "Astra"
  , "Brim"
  , "Coro"
  , "Echo"
  , "Flux"
  , "Gyre"
  , "Helix"
  , "Ion"
  , "Jolt"
  , "Kite"
  , "Lux"
  , "Myrrh"
  , "Nimbus"
  , "Onyx"
  , "Prism"
  ]
suffixes =
  [ "Morph"
  , "Form"
  , "Life"
  , "Cell"
  , "Oid"
  , "Ium"
  , "Ula"
  , "Bit"
  , "Zen"
  , "Pod"
  , "Wave"
  , "Spark"
  , "Mote"
  , "Plex"
  , "Drift"
  , "Strand"
  , "Weave"
  , "Bloom"
  , "Pulse"
  , "Shard"
  , "Gleam"
  , "Trace"
  , "Corpus"
  , "Matrix"
  , "Nexus"
  , "Spore"
  , "Vesicle"
  , "Lattice"
  , "Filament"
  , "Glyph"
  ]
nouns =
  [ "Acuity"
  , "Artifice"
  , "Pallor"
  , "Bloom"
  , "Bifurcation"
  , "Luster"
  , "Vapor"
  , "Wish"
  , "Qualia"
  , "Malady"
  , "Kindred"
  , "Susurrus"
  , "Gossamer"
  , "Subterfuge"
  , "Wretch"
  , "Gibbet"
  , "Murmur"
  , "Flicker"
  , "Shimmer"
  , "Whir"
  , "Pulchritude"
  , "Cadence"
  , "Chroma"
  , "Dialect"
  , "Entropy"
  , "Fractal"
  , "Glimmer"
  , "Horizon"
  , "Inertia"
  , "Juxtaposition"
  , "Kinesis"
  , "Liminal"
  , "Meridian"
  , "Numen"
  , "Obelisk"
  , "Parallax"
  , "Quorum"
  , "Resonance"
  , "Synapse"
  , "Tessera"
  , "Niumbus"
  , "Nebula"
  ]
adjectives =
  [ "Pulchritudinous"
  , "Nascent"
  , "Affine"
  , "Hypoxic"
  , "Ephemeral"
  , "Derelict"
  , "Noetic"
  , "Cogent"
  , "Inveterate"
  , "Laconic"
  , "Mellifluous"
  , "Oblique"
  , "Palimpsest"
  , "Quiescent"
  , "Sanguine"
  , "Tenebrous"
  , "Umbral"
  , "Verdant"
  , "Wistful"
  , "Xenial"
  , "Undead"
  ]
verbsIng =
  [ "Acceding"
  , "Capitulating"
  , "Flickering"
  , "Whirring"
  , "Murmuring"
  , "Exalting"
  , "Shimmering"
  , "Acquiescing"
  , "Languishing"
  , "Blooming"
  , "Wishing"
  , "Vaporing"
  , "Dissolving"
  , "Evolving"
  , "Glimmering"
  , "Orbiting"
  , "Pulsing"
  , "Radiating"
  , "Spiraling"
  , "Unfolding"
  , "Wavering"
  , "Yielding"
  , "Zenithing"
  , "Drifting"
  , "Bleeding"
  , "Tremoring"
  ]
