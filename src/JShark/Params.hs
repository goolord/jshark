{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Type-level parameter rows for n-ary callbacks.
--
-- Row symbols must be unique ('UniqueRow'). Use 'LookupParam' for field
-- lookup errors instead of opaque 'HasField' failures.
--
-- * 'fnLit' / 'ToFn' — positional @function(a,b,…)@ ('Fn').
-- * 'lambdaRow' / 'ToLambda' — curried @'Function@ nest.
module JShark.Params
  ( Param
  , ParamRec (..)
  , RowUs
  , RowFn
  , ParamAt
  , UniqueRow
  , FnFromRow (..)
  , LambdaFromRow (..)
  , fnLit
  , lambdaRow
  , ToFn (..)
  , ToLambda (..)
  )
where

import Data.Kind (Constraint, Type)
import GHC.Records (HasField (..))
import GHC.TypeLits (ErrorMessage (..), Symbol, TypeError)
import JShark.Types (Expr (..), FnBody (..), Universe (..))

-- | One named parameter slot (@'Param "a" 'Number@).
data Param (sym :: Symbol) (u :: Universe)

type family RowUs (row :: [Type]) :: [Universe] where
  RowUs '[] = '[]
  RowUs (Param sym u ': rs) = u ': RowUs rs

-- | Curried @'Function@ type for parameter row @row@ and result @r@.
type family RowFn (row :: [Type]) (r :: Universe) :: Universe where
  RowFn '[] r = r
  RowFn (Param sym u ': rs) r = 'Function u (RowFn rs r)

-- | Universe of the parameter named @sym@ in row @row@.
type family ParamAt (row :: [Type]) (sym :: Symbol) :: Universe where
  ParamAt (Param sym u ': rs) sym = u
  ParamAt (Param sym' u' ': rs) sym = ParamAt rs sym

type family LookupParam (row :: [Type]) (sym :: Symbol) :: Universe where
  LookupParam (Param sym u ': rs) sym = u
  LookupParam (Param sym' u' ': rs) sym = LookupParam rs sym
  LookupParam '[] sym =
    TypeError ('Text "Parameter not in row: " ':<>: 'ShowType sym)

type family SymInRow (sym :: Symbol) (row :: [Type]) :: Bool where
  SymInRow _ '[] = 'False
  SymInRow sym (Param sym u ': _) = 'True
  SymInRow sym (_ ': rs) = SymInRow sym rs

type family RequireUnique (sym :: Symbol) (row :: [Type]) :: Constraint where
  RequireUnique sym row = RequireUniqueGo (SymInRow sym row) sym

type family RequireUniqueGo (found :: Bool) (sym :: Symbol) :: Constraint where
  RequireUniqueGo 'True sym =
    TypeError
      ( 'Text "Duplicate parameter name "
          ':<>: 'ShowType sym
          ':<>: 'Text " in row"
      )
  RequireUniqueGo 'False sym = ()

-- | Every 'Param' symbol in @row@ appears at most once.
type family UniqueRow (row :: [Type]) :: Constraint where
  UniqueRow '[] = ()
  UniqueRow (Param sym u ': rs) = (RequireUnique sym rs, UniqueRow rs)

data ParamRec f row where
  ParamRecNil :: ParamRec f '[]
  ParamRecCons :: Expr f u -> ParamRec f rs -> ParamRec f (Param sym u ': rs)

instance {-# OVERLAPPING #-} HasField sym (ParamRec f (Param sym u ': rs)) (Expr f u) where
  getField (ParamRecCons e _) = e

instance
  ( HasField sym (ParamRec f rs) (Expr f u)
  , LookupParam rs sym ~ u
  ) =>
  HasField sym (ParamRec f (Param sym' u' ': rs)) (Expr f u)
  where
  getField (ParamRecCons _ rec) = getField @sym rec

class FnFromRow row us | row -> us where
  fnFromRow :: forall f r. (ParamRec f row -> Expr f r) -> FnBody f us r

instance UniqueRow '[] => FnFromRow '[] '[] where
  fnFromRow k = JfNil (k ParamRecNil)

instance
  (FnFromRow row us, UniqueRow (Param sym u ': row)) =>
  FnFromRow (Param sym u ': row) (u ': us)
  where
  fnFromRow k =
    JfCons $ \x ->
      fnFromRow @row (\rec -> k (ParamRecCons (Var x) rec))

class LambdaFromRow row r fn | row r -> fn where
  lambdaFromRow :: forall f. (ParamRec f row -> Expr f r) -> Expr f fn

instance UniqueRow '[] => LambdaFromRow '[] r r where
  lambdaFromRow k = k ParamRecNil

instance
  (LambdaFromRow row r fn, UniqueRow (Param sym u ': row)) =>
  LambdaFromRow (Param sym u ': row) r ('Function u fn)
  where
  lambdaFromRow k =
    Lambda $ \x ->
      lambdaFromRow @row (\rec -> k (ParamRecCons (Var x) rec))

-- | Positional @function(a,b,…)@ from an explicit parameter row.
fnLit ::
  forall row r f.
  FnFromRow row (RowUs row) =>
  (ParamRec f row -> Expr f r)
  -> Expr f ('Fn (RowUs row) r)
fnLit k = FnLit (fnFromRow k)

class ToFn k where
  type ToFnBinder k :: (Universe -> Type)
  type ToFnRow k :: [Type]
  type ToFnResult k :: Universe
  toFn :: k -> Expr (ToFnBinder k) ('Fn (RowUs (ToFnRow k)) (ToFnResult k))

instance forall f a b c. ToFn (Expr f a -> Expr f b -> Expr f c) where
  type ToFnBinder (Expr f a -> Expr f b -> Expr f c) = f
  type ToFnRow (Expr f a -> Expr f b -> Expr f c) = '[Param "a" a, Param "b" b]
  type ToFnResult (Expr f a -> Expr f b -> Expr f c) = c
  toFn g =
    fnLit @('[Param "a" a, Param "b" b]) (\p -> g p.a p.b)

instance forall f a b c d. ToFn (Expr f a -> Expr f b -> Expr f c -> Expr f d) where
  type ToFnBinder (Expr f a -> Expr f b -> Expr f c -> Expr f d) = f
  type
    ToFnRow (Expr f a -> Expr f b -> Expr f c -> Expr f d) =
      '[Param "a" a, Param "b" b, Param "c" c]
  type ToFnResult (Expr f a -> Expr f b -> Expr f c -> Expr f d) = d
  toFn g =
    fnLit @('[Param "a" a, Param "b" b, Param "c" c]) (\p -> g p.a p.b p.c)

class ToLambda k where
  type ToLambdaBinder k :: (Universe -> Type)
  type ToLambdaResult k :: Universe
  toLambda :: k -> Expr (ToLambdaBinder k) (ToLambdaResult k)

instance forall f a b c. ToLambda (Expr f a -> Expr f b -> Expr f c) where
  type ToLambdaBinder (Expr f a -> Expr f b -> Expr f c) = f
  type
    ToLambdaResult (Expr f a -> Expr f b -> Expr f c) =
      'Function a ('Function b c)
  toLambda g =
    lambdaFromRow @('[Param "a" a, Param "b" b]) (\p -> g p.a p.b)

instance forall f a b c d. ToLambda (Expr f a -> Expr f b -> Expr f c -> Expr f d) where
  type ToLambdaBinder (Expr f a -> Expr f b -> Expr f c -> Expr f d) = f
  type
    ToLambdaResult (Expr f a -> Expr f b -> Expr f c -> Expr f d) =
      'Function a ('Function b ('Function c d))
  toLambda g =
    lambdaFromRow @('[Param "a" a, Param "b" b, Param "c" c]) (\p -> g p.a p.b p.c)

lambdaRow ::
  forall row r fn f.
  LambdaFromRow row r fn =>
  (ParamRec f row -> Expr f r)
  -> Expr f fn
lambdaRow = lambdaFromRow
