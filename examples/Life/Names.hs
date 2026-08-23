{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Shared species labels and procedural naming for catalog + discovery.
module Names
  ( patternLabel
  , catalogNamesJson
  , nameOfSid
  , cachedNameOfSid
  , uniqueNameSid
  , refreshTakenNames
  , recordDiscoveredName
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import JShark.Api
import qualified JShark.Array as Array
import qualified JShark.Map as Map
import qualified JShark.Set as Set
import JShark.Types (Effect (Lift), Expr (Literal, Var))
import Patterns (PatternSpec (..), allPatterns)
import Types (discoverMin, manualSpecies, soupSpecies)

patternLabel :: Int -> Text
patternLabel = \case
  1 -> "Block"
  2 -> "Beehive"
  3 -> "Loaf"
  4 -> "Boat"
  5 -> "Tub"
  6 -> "Pond"
  7 -> "Ship"
  8 -> "Long Boat"
  9 -> "Mango"
  10 -> "Hat"
  11 -> "Shillelagh"
  12 -> "Dock"
  13 -> "Barge"
  14 -> "Long Snake"
  15 -> "Cis Hook"
  16 -> "Elevator"
  17 -> "Paperclip"
  18 -> "Table On Table"
  19 -> "Integral Sign"
  20 -> "Hook"
  21 -> "Canoe"
  22 -> "Aircraft Carrier"
  23 -> "Trans Barge"
  24 -> "Cis Fuse"
  25 -> "Blinker"
  26 -> "Toad"
  27 -> "Beacon"
  28 -> "Pulsar"
  29 -> "Pentadecathlon"
  30 -> "Queen Bee"
  31 -> "Figure Eight"
  32 -> "Sparkles"
  33 -> "Unix"
  34 -> "Tumbler"
  35 -> "Tripole"
  36 -> "By Flops"
  37 -> "Mold"
  38 -> "Clock"
  39 -> "Quadpole"
  40 -> "Butterfly"
  41 -> "Traffic Circle"
  42 -> "Pentant"
  43 -> "Crossroads"
  44 -> "Pinwheel"
  45 -> "Glider"
  46 -> "LWSS"
  47 -> "MWSS"
  48 -> "HWSS"
  49 -> "Glider Alt"
  50 -> "LWSS Alt"
  51 -> "Glider Perp"
  52 -> "LWSS Perp"
  53 -> "MWSS Alt"
  54 -> "Dart"
  55 -> "Crab"
  56 -> "Loafer"
  57 -> "Glider Up"
  58 -> "Glider Down"
  59 -> "Glider Left"
  60 -> "R-Pentomino"
  61 -> "Acorn"
  62 -> "Diehard"
  63 -> "Rabbits"
  64 -> "S-Diehard"
  65 -> "B-Heptomino"
  66 -> "Pi-Heptomino"
  67 -> "R-Acorn"
  68 -> "Switch Engine"
  69 -> "Block On Table"
  70 -> "Eater"
  71 -> "Eater 2"
  72 -> "Eater 3"
  73 -> "Block On Snake"
  74 -> "Tub With Tail"
  75 -> "Long Hook With Tail"
  76 -> "Snake Bridge"
  77 -> "Mirrored Eater"
  78 -> "Pre-Block"
  79 -> "Pre-Beehive"
  80 -> "Traffic Light"
  81 -> "Honey Farm"
  82 -> "Farm"
  83 -> "Long Boat Tie"
  84 -> "Cis Long Hook"
  85 -> "Trans Long Hook"
  86 -> "Very Long Boat"
  87 -> "Cis Boat"
  88 -> "Trans Boat"
  89 -> "Cis Block"
  n -> "Type " <> T.pack (show n)

catalogNamesJson :: Text
catalogNamesJson =
  T.concat
    [ "["
    , T.intercalate "," [nameEntry p | p <- allPatterns]
    , "]"
    ]
 where
  nameEntry p =
    let
      sid = patId p
      nm = patternLabel sid
     in
      "[" <> T.pack (show sid) <> "," <> jsonString nm <> "]"

jsonString :: Text -> Text
jsonString t =
  "\"" <> T.concat (map jsonChar (T.unpack t)) <> "\""
 where
  jsonChar '"' = "\\\""
  jsonChar '\\' = "\\\\"
  jsonChar c = T.singleton c

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

wordArray :: [Text] -> Expr f ('Array 'String)
wordArray xs = Literal (ValueArray (map ValueString xs))

prefixesArr, suffixesArr, nounsArr, adjectivesArr, verbsIngArr :: Expr f ('Array 'String)
prefixesArr = wordArray prefixes
suffixesArr = wordArray suffixes
nounsArr = wordArray nouns
adjectivesArr = wordArray adjectives
verbsIngArr = wordArray verbsIng

pickWord ::
  Expr f ('Array 'String)
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'String
pickWord arr n mul shift =
  Array.index arr (rem_ (n * mul + ushr n shift) (Array.length arr))

makeName :: Expr f 'Number -> Expr f 'String
makeName n =
  let
    noun = pickWord nounsArr n (number 11) (number 3)
    mode = rem_ n (number 3)
   in
    if_
      (mode .== 0)
      ( pickWord prefixesArr n (number 17) (number 5)
          <> string " "
          <> pickWord suffixesArr n (number 23) (number 7)
      )
      ( if_
          (mode .== 1)
          ( pickWord verbsIngArr n (number 5) (number 4)
              <> string " "
              <> noun
          )
          ( pickWord adjectivesArr n (number 13) (number 6)
              <> string " "
              <> noun
          )
      )

collectTaken ::
  Effect f ('MutableObject a) -> Effect f ('Set 'String) -> EffectSyntax f (f 'Unit)
collectTaken registry taken = do
  catalogNames <- getProp registry "catalogNames"
  _ <- Map.mapM_ (\_ v -> Set.insert taken v) (Lift catalogNames)
  names <- getProp registry "names"
  _ <- Map.mapM_ (\_ v -> Set.insert taken v) (Lift names)
  done

refreshTakenNames ::
  Effect f ('MutableObject a) -> EffectSyntax f (f 'Unit)
refreshTakenNames registry = do
  taken <- getProp registry "takenNames"
  let takenSet = Lift taken
  _ <- Set.clear takenSet
  collectTaken registry takenSet

recordDiscoveredName ::
  Expr f 'Number
  -> Expr f 'String
  -> Effect f ('MutableObject a)
  -> EffectSyntax f (f 'Unit)
recordDiscoveredName sid nm registry = do
  namesMap <- getProp registry "names"
  _ <- Map.insert (Lift namesMap) sid nm
  taken <- getProp registry "takenNames"
  _ <- Set.insert (Lift taken) nm
  cache <- getProp registry "displayCache"
  _ <- Map.insert (Lift cache) sid nm
  done

uniqueNameSid ::
  Expr f 'Number
  -> Effect f ('MutableObject a)
  -> EffectSyntax f (Expr f 'String)
uniqueNameSid sid registry = bindExpr $ fromSyntax $ do
  taken <- getProp registry "takenNames"
  let takenSet = Lift taken
  let base = makeName sid
  stSym <- toSyntax emptyObject
  let st = Lift (Var stSym)
  _ <- setProp st "candidate" base
  _ <- setProp st "seq" (number 2)
  inTaken <- Set.member takenSet base
  _ <-
    whenS inTaken $ do
      toSyntax_ $
        while_
          ( fromSyntax $ do
              cand <- getProp st "candidate"
              hit <- Set.member takenSet cand
              toSyntax $ expr hit
          )
          ( fromSyntax $ do
              seqN <- getProp st "seq"
              _ <- setProp st "candidate" (base <> string " " <> toString seqN)
              _ <- setProp st "seq" (seqN + 1)
              done
          )
      done
  candidate <- getProp st "candidate"
  toSyntax $ expr candidate

nameOfDiscovered ::
  Expr f 'Number -> Effect f ('MutableObject a) -> EffectSyntax f (f 'String)
nameOfDiscovered sid registry = do
  names <- getProp registry "names"
  discHit <- Map.lookup (Lift names) sid
  toSyntax $
    optionCaseE
      discHit
      ( expr
          ( if_
              (sid .>= number (fromIntegral discoverMin))
              (makeName sid)
              (string "Type " <> toString sid)
          )
      )
      (\nm -> expr nm)

nameOfCatalog ::
  Expr f 'Number -> Effect f ('MutableObject a) -> EffectSyntax f (f 'String)
nameOfCatalog sid registry = do
  catalogNames <- getProp registry "catalogNames"
  catHit <- Map.lookup (Lift catalogNames) sid
  toSyntax $
    optionCaseE
      catHit
      (fromSyntax (nameOfDiscovered sid registry))
      (\nm -> expr nm)

nameOfSid ::
  Expr f 'Number
  -> Effect f ('MutableObject a)
  -> EffectSyntax f (Expr f 'String)
nameOfSid sid registry =
  bindExpr $
    ifE
      (expr (sid .== number (fromIntegral soupSpecies)))
      (expr (string "Soup"))
      ( ifE
          (expr (sid .== number (fromIntegral manualSpecies)))
          (expr (string "Manual"))
          (fromSyntax (nameOfCatalog sid registry))
      )

-- | Like 'nameOfSid' but memoizes on @registry.displayCache@.
cachedNameOfSid ::
  Expr f 'Number
  -> Effect f ('MutableObject a)
  -> EffectSyntax f (Expr f 'String)
cachedNameOfSid sid registry =
  bindExpr $
    fromSyntax $ do
      cache <- getProp registry "displayCache"
      hit <- Map.lookup (Lift cache) sid
      toSyntax $
        optionCaseE
          hit
          (fromSyntax $ do
            nm <- nameOfSid sid registry
            _ <- Map.insert (Lift cache) sid nm
            toSyntax $ expr nm)
          (\nm -> expr nm)
