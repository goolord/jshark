{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Shared species labels and procedural naming for catalog + discovery.
module JShark.Example.Life.Names
  ( lookupDisplayName
  , uniqueNameSid
  , refreshTakenNames
  , recordDiscoveredName
  )
where

import JShark.Api
import JShark.Api.Types (Effect (Lift), Expr (Var))
import qualified JShark.Array as Array
import qualified JShark.Map as Map
import qualified JShark.Set as Set

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

refreshTakenNames :: Effect f ('MutableObject a) -> EffectSyntax f (f 'Unit)
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

lookupDisplayName ::
  Expr f 'Number
  -> Effect f ('MutableObject a)
  -> EffectSyntax f (Expr f 'String)
lookupDisplayName sid registry = do
  cache <- getProp registry "displayCache"
  hit <- Map.lookup (Lift cache) sid
  pure (orElse hit (string "Type " <> toString sid))
