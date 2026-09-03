{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | Canonical shape hashes for catalog patterns. Used at runtime to
--    recognize emergent soup formations as known taxa before minting novel
--    species ids. Static bundles are emitted to @js/catalog.js@ (see
--    'catalogJs').
module JShark.Example.Life.Catalog
  ( shapeHash
  , canonicalShapeHash
  , catalogJs
  , nameWords
  , buildKnownMap
  , buildNamesMap
  , buildDisturbMap
  , catalogInitialCells
  , catalogPrefixes
  , catalogSuffixes
  , catalogNouns
  , catalogAdjectives
  , catalogVerbsIng
  , stampCatalogCells
  )
where

import Data.Char (ord)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import JShark.Api
import qualified JShark.Array as Array
import JShark.Example.Life.Grid (setU8)
import JShark.Example.Life.Names (patternLabel)
import JShark.Example.Life.Patterns
  ( PatternSpec (..)
  , allPatterns
  , disturbPatterns
  , initialCatalogCells
  )
import qualified JShark.Map as Map
import Numeric (showHex)

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

buildKnownMap :: EffectSyntax f (Effect f ('Map 'String 'Number))
buildKnownMap = do
  m <- hold Map.new
  sequence_
    [ Map.insert m (string h) (number (fromIntegral sid))
    | (h, sid) <- knownCatalog
    ]
  pure m

buildNamesMap :: EffectSyntax f (Effect f ('Map 'Number 'String))
buildNamesMap = do
  m <- hold Map.new
  sequence_
    [ Map.insert
        m
        (number (fromIntegral (patId p)))
        (string (patternLabel (patId p)))
    | p <- allPatterns
    ]
  pure m

patternCellsArray :: PatternSpec -> Effect f ('Array ('Array 'Number))
patternCellsArray p =
  Array.fromEffects
    [ Array.fromEffects
        [ expr (number (fromIntegral x))
        , expr (number (fromIntegral y))
        ]
    | (x, y) <- patCells p
    ]

buildDisturbMap ::
  EffectSyntax f (Effect f ('Map 'Number ('Array ('Array 'Number))))
buildDisturbMap = do
  m <- hold Map.new
  sequence_
    [ do
        cells <- bindExpr $ patternCellsArray p
        Map.insert m (number (fromIntegral (patId p))) cells
    | p <- disturbPatterns
    ]
  pure m

catalogInitialCells ::
  forall f. Effect f ('Array ('Array 'Number))
catalogInitialCells =
  Array.fromEffects
    [ Array.fromEffects
        [ expr (number (fromIntegral i))
        , expr (number (fromIntegral w))
        ]
    | (i, w) <- initialCatalogCells
    ]

stampCatalogCells ::
  Expr f 'Uint8Array
  -> Expr f 'Uint8Array
  -> Expr f ('Array ('Array 'Number))
  -> EffectSyntax f (f 'Unit)
stampCatalogCells alive species cells =
  forRange_ (number 0) (Array.length cells) $ \k -> do
    let
      row = Array.index cells k
      i = Array.index row 0
      w = Array.index row 1
    _ <- setU8 alive i (number 1)
    setU8 species i w

catalogPrefixes
  , catalogSuffixes
  , catalogNouns
  , catalogAdjectives
  , catalogVerbsIng ::
    forall f. Effect f ('Array 'String)
catalogPrefixes = Array.fromEffects [expr (string w) | w <- prefixes]
catalogSuffixes = Array.fromEffects [expr (string w) | w <- suffixes]
catalogNouns = Array.fromEffects [expr (string w) | w <- nouns]
catalogAdjectives = Array.fromEffects [expr (string w) | w <- adjectives]
catalogVerbsIng = Array.fromEffects [expr (string w) | w <- verbsIng]

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
