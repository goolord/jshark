{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}

-- | Lower closed PHOAS 'Expr' \/ 'Effect' terms to the first-order IR.
--
-- Binders get negative tags from a counter that steps by 'optStep'. A
-- binder's right-hand side and body share the tags below it, so sibling
-- scopes reuse tags.
module JShark.Compiler.Lower
  ( lowerOptExprIr
  , lowerOptEffectIrWith
  , optimizedEffectSize
  , optimizedExprSize
  , irEffectFromClosed
  )
where

import Control.Monad.State.Strict (State, evalState, get, modify', put)
import Data.Functor.Const (Const (..))
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits (KnownSymbol, symbolVal)
import JShark.Api.Rec
import JShark.Api.Types
import JShark.Compiler.Ir

type Tag :: Universe -> Type
type Tag = Const Int

type L = State Int

fresh :: L Int
fresh = get <* modify' (subtract optStep)

-- | Run without advancing the counter for what follows.
local :: L a -> L a
local m = get >>= \t -> m <* put t

ir :: N Ir -> L Ir
ir = pure . Ir

key :: forall k. KnownSymbol k => Text
key = T.pack (symbolVal (Proxy @k))

lowerE :: Expr Tag u -> L Ir
lowerE = \case
  Literal v -> ir (NLit (SomeValue v))
  Var (Const i) -> ir (NVar i)
  Let h x g ->
    fresh >>= \t -> Ir <$> (NLet t h <$> local (lowerE x) <*> lowerE (g (Const t)))
  LetRec r b ->
    fresh >>= \t -> Ir <$> (NLetRec t <$> lowerE (r (Const t)) <*> lowerE (b (Const t)))
  Lambda info g -> fresh >>= \t -> Ir . NLam t info <$> lowerE (g (Const t))
  Apply f x -> Ir <$> (NApp <$> lowerE f <*> lowerE x)
  If c t e -> Ir <$> (NIf <$> lowerE c <*> lowerE t <*> lowerE e)
  OptionCase o n s -> do
    t <- fresh
    Ir <$> (NOptCase <$> lowerE o <*> lowerE n <*> pure t <*> lowerE (s (Const t)))
  ResultOk x -> Ir . NResOk <$> lowerE x
  ResultErr x -> Ir . NResErr <$> lowerE x
  ResultCase o e k -> do
    te <- fresh
    tk <- fresh
    o' <- lowerE o
    e' <- lowerE (e (Const te))
    Ir . NResCase o' te e' tk <$> lowerE (k (Const tk))
  Index a i -> Ir <$> (NIndex <$> lowerE a <*> lowerE i)
  U8Index a i -> Ir <$> (NU8Index <$> lowerE a <*> lowerE i)
  Error m -> Ir . NError <$> lowerE m
  Std (Fixed op args) ->
    Ir . NFixed (SomeFixedOp op) <$> case args of
      ArgsU x -> sequence [lowerE x]
      ArgsB x y -> sequence [lowerE x, lowerE y]
      ArgsT x y z -> sequence [lowerE x, lowerE y, lowerE z]
  Std (Kernel k) -> lowerK k
  Std (Method m) -> lowerM m
  FnLit body -> do
    ts <- mapM (const fresh) [1 .. depth body]
    let
      go :: [Int] -> FnBody Tag us r -> L ([Maybe Text], Ir)
      go _ (JfNil e) = (,) [] <$> lowerE e
      go (t : rest) (JfCons h k) = (\(hs, b) -> (h : hs, b)) <$> go rest (k (Const t))
      go [] (JfCons {}) = error "JShark.Compiler.Lower: FnLit arity mismatch"
    (hs, b) <- go ts body
    ir (NFnLit ts hs b)
  UnsafeNullable x -> Ir . NNullable <$> lowerE x
  FrozenLit fs -> Ir . NFrozen <$> mapM lowerF fs
  GetField @k o -> Ir . NGetField (key @k) <$> lowerE o
 where
  depth :: FnBody Tag us r -> Int
  depth = \case
    JfNil _ -> 0
    JfCons _ k -> 1 + depth (k (Const minBound))

lowerK :: Kernel Tag u -> L Ir
lowerK = \case
  KConcat x y -> k2 OConcat x y
  KNum op x y -> k2 (ONum op) x y
  KNegate x -> k1 ONeg x
  KAnd x y -> k2 OAnd x y
  KOr x y -> k2 OOr x y
  KEq s x y -> k2 (OEq s) x y
  KNEq s x y -> k2 (ONEq s) x y
  KCmp c x y -> k2 (OCmp c) x y
  KShow x -> k1 OShow x
  KTypeOf x -> k1 OTypeOf x
 where
  k1 op x = Ir . NK1 op <$> lowerE x
  k2 op x y = Ir <$> (NK2 op <$> lowerE x <*> lowerE y)

lowerM :: Method Tag u -> L Ir
lowerM = \case
  MethMap a f -> one MMap a (\t -> f (Const t))
  MethFilter a f -> one MFilter a (\t -> f (Const t))
  MethReduce a z f -> two MReduce a (Just z) f
  MethReduceRight a z f -> two MReduceRight a (Just z) f
  MethToSorted a f -> two MToSorted a Nothing f
  MethFrom n f -> one MFrom n (\t -> f (Const t))
 where
  one m a body = do
    t <- fresh
    a' <- lowerE a
    Ir . NMeth m a' Nothing [t] <$> lowerE (body t)
  two ::
    Meth
    -> Expr Tag x
    -> Maybe (Expr Tag z)
    -> (Tag p -> Tag q -> Expr Tag w)
    -> L Ir
  two m a z body = do
    ta <- fresh
    tb <- fresh
    a' <- lowerE a
    z' <- traverse lowerE z
    Ir . NMeth m a' z' [ta, tb] <$> lowerE (body (Const ta) (Const tb))

lowerF :: FieldLit Tag r -> L (IrField Ir)
lowerF = \case
  FieldLit @k a -> IrField False (key @k) <$> lowerArg a
  FieldLitExtra @k a -> IrField True (key @k) <$> lowerArg a

lowerArg :: Arg Tag u -> L Ir
lowerArg = \case
  ArgExpr e -> lowerE e
  ArgEffect e -> lowerX e

lowerArgs :: Rec (Arg Tag) us -> L [Ir]
lowerArgs = \case
  RecNil -> pure []
  RecCons a rest -> (:) <$> lowerArg a <*> lowerArgs rest

lowerX :: Effect Tag u -> L Ir
lowerX = \case
  Lift x -> Ir . NLift <$> lowerE x
  FFI n args -> Ir . NFFI n <$> lowerArgs args
  UnsafeObject o -> ir (NUObj o)
  UnsafeObjectGet x s -> Ir . (`NUGet` s) <$> lowerX x
  UnsafeObjectAssign x y -> Ir <$> (NUSet <$> lowerX x <*> lowerX y)
  CallMethod x n args -> Ir <$> (NCall <$> lowerX x <*> pure n <*> lowerArgs args)
  Bind h x f ->
    fresh >>= \t -> Ir <$> (NBind t h <$> local (lowerX x) <*> lowerX (f (Const t)))
  ThenE x y -> Ir <$> (NThen <$> lowerX x <*> lowerX y)
  BindRec r b ->
    fresh >>= \t -> Ir <$> (NBindRec t <$> lowerX (r (Const t)) <*> lowerX (b (Const t)))
  LambdaE f -> fresh >>= \t -> Ir . NLamE t <$> lowerX (f (Const t))
  ApplyE f x -> Ir <$> (NAppE <$> lowerX f <*> lowerX x)
  IfE c t e -> Ir <$> (NIfE <$> lowerX c <*> lowerX t <*> lowerX e)
  While c b -> Ir <$> (NWhile <$> lowerX c <*> lowerX b)
  ForRange s e f -> do
    t <- fresh
    Ir <$> (NFor <$> lowerE s <*> lowerE e <*> pure t <*> lowerX (f (Const t)))
  U8Set b i v -> Ir <$> (NU8Set <$> lowerE b <*> lowerE i <*> lowerE v)
  U8Fill b v -> Ir <$> (NU8Fill <$> lowerE b <*> lowerE v)
  OptionCaseE o n s -> do
    t <- fresh
    Ir <$> (NOptCaseE <$> lowerE o <*> lowerX n <*> pure t <*> lowerX (s (Const t)))
  ResultCaseE o e k -> do
    te <- fresh
    tk <- fresh
    o' <- lowerE o
    e' <- lowerX (e (Const te))
    Ir . NResCaseE o' te e' tk <$> lowerX (k (Const tk))
  StringCaseE s arms d ->
    Ir <$> (NStrCase <$> lowerE s <*> mapM (traverse lowerX) arms <*> lowerX d)
  Throw x -> Ir . NThrow <$> lowerE x
  Try a k ->
    fresh >>= \t -> Ir <$> (NTry <$> lowerX a <*> pure t <*> lowerX (k (Const t)))
  ObjectLit fs -> Ir . NObjLit <$> mapM lowerF fs
  DeleteProp o k -> Ir <$> (NDelete <$> lowerX o <*> lowerE k)
  ArrayLit es -> Ir . NArray <$> mapM lowerX es

run :: (?keepLets :: Bool) => L Ir -> (Ir, Int)
run m = let (o, md) = optIr (evalState m (-2)) in (o, mSize md)

-- | Lower and optimize a closed expression; also returns the optimized size.
lowerOptExprIr :: Bool -> ClosedExpr u -> (Ir, Int)
lowerOptExprIr keepLets e = let ?keepLets = keepLets in run (lowerE e)

-- | Lower and optimize a closed effect; also returns the optimized size.
lowerOptEffectIrWith :: Bool -> ClosedEffect u -> (Ir, Int)
lowerOptEffectIrWith keepLets e = let ?keepLets = keepLets in run (lowerX e)

irEffectFromClosed :: ClosedEffect u -> Ir
irEffectFromClosed e = fst (lowerOptEffectIrWith False e)

-- | Nodes after optimizing a closed effect.
optimizedEffectSize :: ClosedEffect u -> Int
optimizedEffectSize e = snd (lowerOptEffectIrWith False e)

-- | Nodes after optimizing a closed expression.
optimizedExprSize :: ClosedExpr u -> Int
optimizedExprSize e = snd (lowerOptExprIr False e)
