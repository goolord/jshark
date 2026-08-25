{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Shared species labels and procedural naming for catalog + discovery.
module Names
  ( patternLabel
  , nameOfSid
  , cachedNameOfSid
  , lookupDisplayName
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
import JShark.Types (Effect (Lift), Expr (Var))
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
  63 -> "Bunnies"
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

pickWord ::
  Expr f ('Array 'String)
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'Number
  -> Expr f 'String
pickWord arr n mul shift =
  Array.index arr (rem_ (n * mul + ushr n shift) (Array.length arr))

makeName ::
  Effect f ('MutableObject a)
  -> Expr f 'Number
  -> EffectSyntax f (Expr f 'String)
makeName registry n = do
  nouns <- getProp registry "nouns"
  prefixes <- getProp registry "prefixes"
  suffixes <- getProp registry "suffixes"
  verbsIng <- getProp registry "verbsIng"
  adjectives <- getProp registry "adjectives"
  let
    noun = pickWord nouns n (number 11) (number 3)
    mode = rem_ n (number 3)
  pure
    ( if_
        (mode .== 0)
        ( pickWord prefixes n (number 17) (number 5)
            <> string " "
            <> pickWord suffixes n (number 23) (number 7)
        )
        ( if_
            (mode .== 1)
            ( pickWord verbsIng n (number 5) (number 4)
                <> string " "
                <> noun
            )
            ( pickWord adjectives n (number 13) (number 6)
                <> string " "
                <> noun
            )
        )
    )

makeNameEffect ::
  Effect f ('MutableObject a) -> Expr f 'Number -> Effect f 'String
makeNameEffect registry sid =
  fromSyntax $ do
    nm <- makeName registry sid
    toSyntax (expr nm)

collectTaken ::
  Effect f ('MutableObject a)
  -> Effect f ('Set 'String)
  -> EffectSyntax f (f 'Unit)
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
  let
    takenSet = Lift taken
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
  let
    takenSet = Lift taken
  base <- makeName registry sid
  stSym <- toSyntax emptyObject
  let
    st = Lift (Var stSym)
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
  fallback <-
    bindExpr $
      ifE
        (expr (sid .>= number (fromIntegral discoverMin)))
        (makeNameEffect registry sid)
        (expr (string "Type " <> toString sid))
  toSyntax $
    optionCaseE
      discHit
      (expr fallback)
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

lookupDisplayName ::
  Expr f 'Number
  -> Effect f ('MutableObject a)
  -> EffectSyntax f (Expr f 'String)
lookupDisplayName sid registry = do
  cache <- getProp registry "displayCache"
  hit <- Map.lookup (Lift cache) sid
  pure (orElse hit (string "Type " <> toString sid))

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
          ( fromSyntax $ do
              nm <- nameOfSid sid registry
              _ <- Map.insert (Lift cache) sid nm
              toSyntax $ expr nm
          )
          (\nm -> expr nm)
