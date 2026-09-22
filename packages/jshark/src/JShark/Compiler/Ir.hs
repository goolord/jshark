{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}

-- | First-order IR and its optimizer. PHOAS 'Expr' \/ 'Effect' terms lower
-- here once ("JShark.Compiler.Lower"); codegen numbers the optimized tree.
--
-- One node functor 'N' carries both expression and effect constructors.
-- Expression\/effect /position/ is a property of the top constructor
-- ('isEffectNode'). Binders are negative 'Int' tags.
--
-- Internal to the JShark compiler; exposed through "JShark.Internal" for
-- tests and tooling, and its API may change between 0.x releases.
module JShark.Compiler.Ir
  ( N (..)
  , Ir (..)
  , IrField (..)
  , FieldKind (..)
  , Op1 (..)
  , Op2 (..)
  , Meth (..)
  , SomeValue (..)
  , SomeFixedOp (..)
  , Meta (..)
  , data LitV
  , isEffectNode
  , scoped
  , metaIr
  , optIr
  , occursIr
  , validateIr
  , optStep
  )
where

import Data.Bits (xor, (.&.), (.|.))
import Data.Foldable (toList)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Maybe (fromMaybe, isJust)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits (symbolVal)
import JShark.Api.Prim
import JShark.Api.Types
  ( BigBinOp
  , Expr (Literal)
  , FFIForm
  , FieldLit (..)
  , FixedOp (..)
  , LamInfo (..)
  , Value (..)
  )
import JShark.Compiler.Emit (jsBit2, jsRem, jsShl, jsShr, jsUShr)
import JShark.Compiler.Evaluate
  ( isCheapValue
  , jsShow
  , keepLastByKey
  , parseBigIntString
  , tryEvalBigBin
  , typeOfValue
  )

-- | A 'Value' with its universe hidden.
data SomeValue where
  SomeValue :: Value u -> SomeValue

-- | A 'FixedOp' with its universes hidden.
data SomeFixedOp where
  SomeFixedOp :: FixedOp a b c u -> SomeFixedOp

-- | Declared or out-of-row ('FExtra') object field, pure or effectful.
data FieldKind = FPlain | FEff | FExtra | FExtraEff
  deriving Eq

data IrField r = IrField !FieldKind !Text r
  deriving (Functor, Foldable, Traversable)

-- | Binary kernel operators. The 'Bool' on 'OEq' \/ 'ONEq' selects the
-- structural @$valueEq@ shim over @===@.
data Op2
  = OConcat
  | OPlus
  | OTimes
  | OMinus
  | ODiv
  | ORem
  | OBitAnd
  | OBitOr
  | OBitXor
  | OShl
  | OShr
  | OUShr
  | OBig BigBinOp
  | OAnd
  | OOr
  | OEq Bool
  | ONEq Bool
  | OGTh
  | OLTh
  | OGTEq
  | OLTEq

data Op1 = ONeg | OBigNeg | OShow | OTypeOf

data Meth = MMap | MFilter | MReduce | MReduceRight | MToSorted | MFrom
  deriving Eq

-- | One IR node over children @r@. Children are listed in evaluation order;
-- the derived 'Traversable' visits them in that order.
data N r
  = NLit SomeValue
  | NVar !Int
  | NLet !Int (Maybe Text) r r
  | NLetRec !Int r r
  | NLam !Int LamInfo r
  | NApp r r
  | NIf r r r
  | NOptCase r r !Int r
  | NResOk r
  | NResErr r
  | NResCase r !Int r !Int r
  | NIndex r r
  | NU8Index r r
  | NError r
  | NFixed SomeFixedOp [r]
  | NFnLit [Int] [Maybe Text] r
  | NNullable r
  | NFrozen [IrField r]
  | NGetField Text r
  | NK2 Op2 r r
  | NK1 Op1 r
  | -- | Array method: receiver, @reduce@ seed, callback binders, body.
    NMeth Meth r (Maybe r) [Int] r
  | NLift r
  | NFFI FFIForm [r]
  | NUObj Text
  | NUGet r Text
  | NUSet r r
  | NCall r Text [r]
  | NBind !Int (Maybe Text) r r
  | NThen r r
  | NBindRec !Int r r
  | NLamE !Int r
  | NAppE r r
  | NIfE r r r
  | NWhile r r
  | NFor r r !Int r
  | NU8Set r r r
  | NU8Fill r r
  | NOptCaseE r r !Int r
  | NResCaseE r !Int r !Int r
  | NStrCase r [(Text, r)] r
  | NThrow r
  | NTry r !Int r
  | NObjLit [IrField r]
  | NDelete r r
  | NArray [r]
  | -- | Codegen only: an effect in expression position.
    NEmbed r
  deriving (Functor, Foldable, Traversable)

newtype Ir = Ir (N Ir)

pattern LitV :: Value u -> N r
pattern LitV v <- NLit (SomeValue v)

isEffectNode :: N r -> Bool
isEffectNode = \case
  NLift {} -> True
  NFFI {} -> True
  NUObj {} -> True
  NUGet {} -> True
  NUSet {} -> True
  NCall {} -> True
  NBind {} -> True
  NThen {} -> True
  NBindRec {} -> True
  NLamE {} -> True
  NAppE {} -> True
  NIfE {} -> True
  NWhile {} -> True
  NFor {} -> True
  NU8Set {} -> True
  NU8Fill {} -> True
  NOptCaseE {} -> True
  NResCaseE {} -> True
  NStrCase {} -> True
  NThrow {} -> True
  NTry {} -> True
  NObjLit {} -> True
  NDelete {} -> True
  NArray {} -> True
  _ -> False

-- | Observable effects: may not move, be dropped, or be duplicated.
isImpure :: N r -> Bool
isImpure = \case
  NFFI {} -> True
  NUObj {} -> True
  NUGet {} -> True
  NUSet {} -> True
  NCall {} -> True
  NAppE {} -> True
  NWhile {} -> True
  NFor {} -> True
  NU8Set {} -> True
  NU8Fill {} -> True
  NThrow {} -> True
  NTry {} -> True
  NDelete {} -> True
  NError {} -> True
  _ -> False

-- | Each child paired with the binder tags in scope over it.
scoped :: N r -> N ([Int], r)
scoped = \case
  NLet t h x b -> NLet t h ([], x) ([t], b)
  NBind t h x b -> NBind t h ([], x) ([t], b)
  NLetRec t r b -> NLetRec t ([t], r) ([t], b)
  NBindRec t r b -> NBindRec t ([t], r) ([t], b)
  NLam t i b -> NLam t i ([t], b)
  NLamE t b -> NLamE t ([t], b)
  NFnLit ts ns b -> NFnLit ts ns (ts, b)
  NOptCase o n t s -> NOptCase ([], o) ([], n) t ([t], s)
  NOptCaseE o n t s -> NOptCaseE ([], o) ([], n) t ([t], s)
  NResCase o te e to k -> NResCase ([], o) te ([te], e) to ([to], k)
  NResCaseE o te e to k -> NResCaseE ([], o) te ([te], e) to ([to], k)
  NMeth m a z ts b -> NMeth m ([], a) (fmap ((,) []) z) ts (ts, b)
  NFor s e t b -> NFor ([], s) ([], e) t ([t], b)
  NTry a t k -> NTry ([], a) t ([t], k)
  n -> fmap ((,) []) n

-- | Children evaluated exactly once where they stand, and children that are
-- not (function bodies, conditional arms, loop bodies, @&&@ right operands).
lazySplit :: N r -> ([r], [r])
lazySplit = \case
  NLam _ _ b -> ([], [b])
  NLamE _ b -> ([], [b])
  NFnLit _ _ b -> ([], [b])
  NIf c t e -> ([c], [t, e])
  NIfE c t e -> ([c], [t, e])
  NOptCase o n _ s -> ([o], [n, s])
  NOptCaseE o n _ s -> ([o], [n, s])
  NResCase o _ e _ k -> ([o], [e, k])
  NResCaseE o _ e _ k -> ([o], [e, k])
  NK2 OAnd x y -> ([x], [y])
  NK2 OOr x y -> ([x], [y])
  NMeth _ a z _ b -> (a : toList z, [b])
  NWhile c b -> ([], [c, b])
  NFor s e _ b -> ([s, e], [b])
  NStrCase s arms d -> ([s], map snd arms ++ [d])
  NTry a _ k -> ([a], [k])
  n -> (toList n, [])

-- Metadata --------------------------------------------------------------------

-- | Optimizer metadata: size, free-variable use counts, and whether the term
-- may be moved across other evaluations ('mMove'), dropped when unused
-- ('mDrop'), or duplicated cheaply ('mCheap').
data Meta = Meta
  { mSize :: !Int
  , mFree :: !(IntMap Int)
  , mMove :: !Bool
  , mDrop :: !Bool
  , mCheap :: !Bool
  }

instance Semigroup Meta where
  Meta s1 f1 m1 d1 c1 <> Meta s2 f2 m2 d2 c2 =
    Meta (s1 + s2) (IM.unionWith (+) f1 f2) (m1 && m2) (d1 && d2) (c1 && c2)

instance Monoid Meta where
  mempty = Meta 0 IM.empty True True True

-- | Tag spacing for lowering, and the small-body inline threshold.
optStep, optSmall :: Int
optStep = 2
optSmall = 16

leaf :: Bool -> Bool -> Bool -> Meta
leaf = Meta 1 IM.empty

litMeta :: Value u -> Meta
litMeta v = leaf True True (isCheapValue v)

varMeta :: Int -> Meta
varMeta i = Meta 1 (IM.singleton i 1) True True True

effectMd :: Meta -> Meta
effectMd md = md <> Meta 0 IM.empty False False False

nodeMeta :: Meta -> Meta -> Meta
nodeMeta a b = let m = a <> b in m {mSize = mSize m + 1, mCheap = False}

chain :: [Meta] -> Meta
chain = \case
  [] -> mempty
  [m] -> m
  m : ms -> nodeMeta m (chain ms)

bindMeta :: Int -> Meta -> Meta
bindMeta tag md = md {mFree = IM.delete tag (mFree md)}

fixedKeep :: FixedOp a b c u -> Meta
fixedKeep op = leaf (isMoveFixed op) (isDropFixed op) False

-- | Per-node flags: effects, and mutable reads that may not move.
post :: N r -> Meta -> Meta
post n md = case n of
  NU8Index {} -> md {mMove = False}
  NIndex {} -> md {mMove = False, mDrop = False}
  _ | isImpure n -> effectMd md
  _ -> md

-- | Structural metadata of an unoptimized subtree. Every child contributes
-- once and binders are not closed.
metaIr :: Ir -> Meta
metaIr (Ir n) = case n of
  LitV v -> litMeta v
  NVar i -> varMeta i
  NFixed (SomeFixedOp op) _ -> fixedKeep op <> kids
  _ -> post n (leaf True True False) <> kids
 where
  kids = foldl' (\acc c -> acc <> metaIr c) mempty (toList n)

-- Substitution ----------------------------------------------------------------

occursIr :: Int -> Ir -> Bool
occursIr t (Ir n) = case n of
  NVar i -> i == t
  _ -> any (occursIr t) n

-- | Does the tag occur where it is not evaluated exactly once?
lazyOccursIr :: Int -> Ir -> Bool
lazyOccursIr t (Ir n) =
  let (s, l) = lazySplit n in any (lazyOccursIr t) s || any (occursIr t) l

substIr :: Int -> Int -> Ir -> Ir
substIr old new ir@(Ir n)
  | old == new || not (occursIr old ir) = ir
  | NVar _ <- n = Ir (NVar new)
  | otherwise = Ir (fmap (substIr old new) n)

inlineIr :: Int -> Ir -> Ir -> Ir
inlineIr tag bound body = case bound of
  Ir (NVar i) -> substIr tag i body
  _ -> replace body
 where
  replace (Ir n) = case n of
    NVar i | i == tag -> bound
    _ -> Ir (fmap replace n)

-- | Every 'NVar' bound and every binder fresh along its scope path.
validateIr :: Ir -> [Text]
validateIr = go []
 where
  go sc (Ir n) = case n of
    NVar i -> [msg "unbound variable #" i | i `notElem` sc]
    _ -> concat [under tags sc c | (tags, c) <- toList (scoped n)]
  under [] sc c = go sc c
  under (t : ts) sc c = [msg "duplicate binder #" t | t `elem` sc] ++ under ts (t : sc) c
  msg s i = T.pack (s ++ show i)

-- Binder elimination ----------------------------------------------------------

isAliasEffect :: Ir -> Bool
isAliasEffect = \case
  Ir (NLift (Ir (NVar _))) -> True
  Ir (NLift (Ir (NNullable (Ir (NVar _))))) -> True
  _ -> False

isVarOf :: Int -> Ir -> Bool
isVarOf t = \case
  Ir (NVar i) -> i == t
  _ -> False

-- | Only a closed lambda may hoist to a shared @$name@ binding.
closeHoist :: Int -> LamInfo -> Meta -> LamInfo
closeHoist tag info md
  | isJust (lamTag info)
  , not (IM.null (mFree (bindMeta tag md))) =
      info {lamTag = Nothing}
  | otherwise = info

-- | Shared let\/bind eliminator: drop, inline, or keep the binding.
elimBinder ::
  (?keepLets :: Bool) =>
  Bool -> Meta -> Maybe Text -> Int -> Ir -> Ir -> Meta -> (Ir, Meta)
elimBinder eff mdX hint tag x body mdBody = case uses of
  0 | mDrop mdX, not (eff && isAliasEffect x) -> (body, closed)
  0 -> (if eff then Ir (NThen x body) else kept, nodeMeta mdX closed)
  1 | ?keepLets && keep -> (kept, nodeMeta mdX closed)
  1 | mSize mdBody <= optSmall, once -> (inlineIr tag x body, closed <> mdX)
  _ | mCheap mdX, mSize mdBody <= optSmall -> (inlineIr tag x body, closed <> mdX)
  _ -> (kept, nodeMeta mdX closed)
 where
  uses = IM.findWithDefault 0 tag (mFree mdBody)
  closed = bindMeta tag mdBody
  kept = Ir (if eff then NBind tag hint x body else NLet tag hint x body)
  -- A single use inlines only when the bound term may move to that use.
  once =
    (eff && isAliasEffect x)
      || (mMove mdX && (mCheap mdX || not (lazyOccursIr tag body)))
  keep
    | eff = not (isLamE x) && not (isIdentE body) && not (isAliasEffect x)
    | otherwise = not (isLam x) && not (isVarOf tag body)
  isLam = \case Ir NLam {} -> True; _ -> False
  isLamE = \case Ir NLamE {} -> True; _ -> False
  isIdentE = \case Ir (NLift v) -> isVarOf tag v; _ -> False

-- Optimizer -------------------------------------------------------------------

-- | Bottom-up optimize: constant folds, known-constructor cases, and
-- let\/bind elimination. @?keepLets@ keeps single-use named bindings.
optIr :: (?keepLets :: Bool) => Ir -> (Ir, Meta)
optIr ir@(Ir n) = case n of
  LitV v -> (ir, litMeta v)
  NVar i -> (ir, varMeta i)
  NUObj _ -> (ir, leaf False False False)
  NLet tag h x b -> letLike False tag h x b
  NBind tag h x b -> letLike True tag h x b
  NApp (Ir (NLam tag info g)) x ->
    let
      (x', mx) = optIr x
      (g', mg) = optIr g
     in
      case lamTag info of
        -- Named hoists stay calls so codegen emits one shared helper.
        Just _ -> (Ir (NApp (Ir (NLam tag (closeHoist tag info mg) g')) x'), nodeMeta mx mg)
        Nothing -> elimBinder False mx Nothing tag x' g' mg
  NLam tag info g ->
    let
      (g', m) = optIr g
     in
      (Ir (NLam tag (closeHoist tag info m) g'), bindMeta tag m)
  NIf c t e -> ifLike False NIf c t e
  NIfE c t e -> ifLike True NIfE c t e
  NOptCase o nb tag s -> optCase False o nb tag s (\o' n' s' -> NOptCase o' n' tag s')
  NOptCaseE o nb tag s -> optCase True o nb tag s (\o' n' s' -> NOptCaseE o' n' tag s')
  NResCase o te e to k -> resCase False o te e to k (\o' e' k' -> NResCase o' te e' to k')
  NResCaseE o te e to k -> resCase True o te e to k (\o' e' k' -> NResCaseE o' te e' to k')
  NK2 OAnd x y -> andOr True x y
  NK2 OOr x y -> andOr False x y
  NWhile c b ->
    let
      (c', mc) = optIr c
     in
      case c' of
        Ir (NLift (Ir (LitV (ValueBool False)))) ->
          (Ir (NLift (Ir (NLit (SomeValue ValueUnit)))), litMeta ValueUnit <> mc)
        _ -> let (b', mb) = optIr b in (Ir (NWhile c' b'), effectMd (nodeMeta mc mb))
  NAppE f x ->
    let
      (f', mf) = optIr f
     in
      case f' of
        Ir (NLamE tag g) ->
          let
            (x', mx) = optIr x; (g', mg) = optIr g
           in
            elimBinder True mx Nothing tag x' g' mg
        _ -> let (x', mx) = optIr x in (Ir (NAppE f' x'), effectMd (nodeMeta mf mx))
  NStrCase s arms d ->
    let
      (s', ms) = optIr s
     in
      case s' of
        Ir (LitV (ValueString k)) -> optIr (fromMaybe d (lookup k arms))
        _ ->
          let
            arms' = [(k, optIr e) | (k, e) <- arms]
            (d', md) = optIr d
           in
            ( Ir (NStrCase s' [(k, e) | (k, (e, _)) <- arms'] d')
            , nodeMeta ms (nodeMeta (foldMap (snd . snd) arms') md)
            )
  _ ->
    let
      nm = fmap optIr n
      n' = fmap fst nm
      ms = [foldr bindMeta m tags | (tags, (_, m)) <- toList (scoped nm)]
     in
      finish n' ms

-- | Combine optimized children, folding where the children allow it.
finish :: (?keepLets :: Bool) => N Ir -> [Meta] -> (Ir, Meta)
finish n ms = case n of
  NIndex (Ir (LitV (ValueArray vs))) (Ir (LitV (ValueNumber d)))
    | isFiniteDouble d
    , let
        i = truncate d :: Int
    , i >= 0 && i < length vs ->
        let v = vs !! i in (Ir (NLit (SomeValue v)), litMeta v <> mconcat ms)
  NK2 op x y -> folded (fold2 op x y) (leaf True True False)
  NK1 op x -> folded (fold1 op x) (leaf True True False)
  NFixed (SomeFixedOp op) xs -> folded (foldFixed op xs) (fixedKeep op)
  NGetField k (Ir (NFrozen fs))
    | [mo] <- ms
    , mDrop mo
    , Just f <- lookup k [(k', c) | IrField FPlain k' c <- reverse fs] ->
        optIr f
  NFrozen _ -> (Ir n, mconcat ms)
  NObjLit _ -> (Ir n, mconcat ms)
  NArray _ -> (Ir n, mconcat ms)
  NFFI _ _ -> (Ir n, effectMd (argsChain ms))
  NCall {} | mx : rest <- ms -> (Ir n, effectMd (nodeMeta mx (argsChain rest)))
  _ -> (Ir n, post n (chain ms))
 where
  argsChain = foldr nodeMeta mempty
  folded res keepMd = case res of
    Just v@(SomeValue lv) -> (Ir (NLit v), litMeta lv <> chain ms)
    Nothing -> (Ir n, keepMd <> chain ms)

letLike ::
  (?keepLets :: Bool) => Bool -> Int -> Maybe Text -> Ir -> Ir -> (Ir, Meta)
letLike eff tag h x b =
  let (x', mx) = optIr x; (b', mb) = optIr b in elimBinder eff mx h tag x' b' mb

ifLike ::
  (?keepLets :: Bool) =>
  Bool -> (Ir -> Ir -> Ir -> N Ir) -> Ir -> Ir -> Ir -> (Ir, Meta)
ifLike eff mk c t e =
  let
    (c', mc) = optIr c
   in
    case (eff, c') of
      (False, Ir (LitV (ValueBool b))) -> optIr (if b then t else e)
      (True, Ir (NLift (Ir (LitV (ValueBool b))))) -> optIr (if b then t else e)
      _ ->
        let
          (t', mt) = optIr t; (e', me) = optIr e
         in
          (Ir (mk c' t' e'), nodeMeta mc (nodeMeta mt me))

-- | Bind a known case payload in the arm: pure arms by let, effect arms by
-- bind of the lifted payload.
bindPayload :: (?keepLets :: Bool) => Bool -> Int -> Ir -> Ir -> (Ir, Meta)
bindPayload eff tag v arm =
  let
    (arm', ma) = optIr arm
   in
    elimBinder eff (metaIr v) Nothing tag (if eff then Ir (NLift v) else v) arm' ma

optCase ::
  (?keepLets :: Bool) =>
  Bool -> Ir -> Ir -> Int -> Ir -> (Ir -> Ir -> Ir -> N Ir) -> (Ir, Meta)
optCase eff o nb tag s mk =
  let
    (o', mo) = optIr o
   in
    case o' of
      Ir (LitV (ValueOption Nothing)) -> optIr nb
      Ir (LitV (ValueOption (Just v))) -> bindPayload eff tag (Ir (NLit (SomeValue v))) s
      -- Host literals are never JS null; vars and FFI results stay checked.
      Ir (NNullable l@(Ir (NLit _))) -> bindPayload eff tag l s
      _ ->
        let
          (n', mn) = optIr nb; (s', ms) = optIr s
         in
          (Ir (mk o' n' s'), nodeMeta mo (nodeMeta mn (bindMeta tag ms)))

resCase ::
  (?keepLets :: Bool) =>
  Bool -> Ir -> Int -> Ir -> Int -> Ir -> (Ir -> Ir -> Ir -> N Ir) -> (Ir, Meta)
resCase eff o te e to k mk =
  let
    (o', mo) = optIr o
   in
    case o' of
      Ir (LitV (ValueResult (Left v))) -> bindPayload eff te (Ir (NLit (SomeValue v))) e
      Ir (LitV (ValueResult (Right v))) -> bindPayload eff to (Ir (NLit (SomeValue v))) k
      Ir (NResErr x) -> bindPayload eff te x e
      Ir (NResOk x) -> bindPayload eff to x k
      _ ->
        let
          (e', me) = optIr e; (k', mk') = optIr k
         in
          (Ir (mk o' e' k'), nodeMeta mo (nodeMeta (bindMeta te me) (bindMeta to mk')))

-- | @&&@ \/ @||@: fold a literal left operand; a literal right operand folds
-- only when dropping the left one is unobservable.
andOr :: (?keepLets :: Bool) => Bool -> Ir -> Ir -> (Ir, Meta)
andOr isAnd x y =
  let
    (x', mx) = optIr x
    zero = Ir (NLit (SomeValue (ValueBool (not isAnd))))
    zeroMd = litMeta (ValueBool (not isAnd)) <> mx
   in
    case x' of
      Ir (LitV (ValueBool b))
        | b == isAnd -> optIr y
        | otherwise -> (zero, zeroMd)
      _ ->
        let
          (y', my) = optIr y
         in
          case y' of
            Ir (LitV (ValueBool b))
              | b == isAnd -> (x', mx)
              | mDrop mx -> (zero, zeroMd)
            _ ->
              ( Ir (NK2 (if isAnd then OAnd else OOr) x' y')
              , leaf True True False <> nodeMeta mx my
              )

-- Constant folding ------------------------------------------------------------

litOf :: Ir -> Maybe SomeValue
litOf = \case
  Ir (NLit v) -> Just v
  _ -> Nothing

num2 :: (Double -> Double -> Double) -> Ir -> Ir -> Maybe SomeValue
num2 f (Ir (LitV (ValueNumber a))) (Ir (LitV (ValueNumber b))) = Just (SomeValue (ValueNumber (f a b)))
num2 _ _ _ = Nothing

fold2 :: Op2 -> Ir -> Ir -> Maybe SomeValue
fold2 op x y = case op of
  OConcat
    | Ir (LitV (ValueString a)) <- x
    , Ir (LitV (ValueString b)) <- y ->
        Just (SomeValue (ValueString (a <> b)))
  OPlus -> num2 (+) x y
  OTimes -> num2 (*) x y
  OMinus -> num2 (-) x y
  ODiv -> num2 (/) x y
  ORem -> num2 jsRem x y
  OBitAnd -> num2 (jsBit2 (.&.)) x y
  OBitOr -> num2 (jsBit2 (.|.)) x y
  OBitXor -> num2 (jsBit2 xor) x y
  OShl -> num2 jsShl x y
  OShr -> num2 jsShr x y
  OUShr -> num2 jsUShr x y
  OBig b
    | Ir (LitV (ValueBigInt a)) <- x
    , Ir (LitV (ValueBigInt c)) <- y ->
        SomeValue . ValueBigInt <$> tryEvalBigBin b a c
  OEq _ -> bool <$> eqFold x y
  ONEq _ -> bool . not <$> eqFold x y
  OGTh -> ordFold (== GT)
  OLTh -> ordFold (== LT)
  OGTEq -> ordFold (/= LT)
  OLTEq -> ordFold (/= GT)
  _ -> Nothing
 where
  bool = SomeValue . ValueBool
  ordFold cmp = do
    SomeValue a <- litOf x
    SomeValue b <- litOf y
    bool . cmp <$> sameFamilyOrd a b

fold1 :: Op1 -> Ir -> Maybe SomeValue
fold1 op (Ir x) = case (op, x) of
  (ONeg, LitV (ValueNumber a)) -> Just (SomeValue (ValueNumber (negate a)))
  (OBigNeg, LitV (ValueBigInt a)) -> Just (SomeValue (ValueBigInt (negate a)))
  (OShow, LitV (ValueFunction _)) -> Nothing
  (OShow, LitV v) -> Just (SomeValue (ValueString (jsShow v)))
  (OTypeOf, LitV v) -> Just (SomeValue (ValueString (typeOfValue v)))
  _ -> Nothing

foldFixed :: FixedOp a b c u -> [Ir] -> Maybe SomeValue
foldFixed op args = case (op, map litOf args) of
  (_, [Just (SomeValue (ValueNumber a))])
    | Just (MathUnary op') <- matchMathUnary op -> num <$> exactMathUnary op' a
  (_, [Just (SomeValue (ValueNumber a)), Just (SomeValue (ValueNumber b))])
    | Just (MathBinary op') <- matchMathBinary op -> num <$> exactMathBinary op' a b
  (FixArrLen, [Just (SomeValue (ValueArray vs))]) -> Just (num (fromIntegral (length vs)))
  (FixToBigInt, [Just (SomeValue (ValueNumber d))])
    | isFiniteDouble d
    , d == fromInteger (truncate d) ->
        Just (SomeValue (ValueBigInt (truncate d)))
  (FixFromBigInt, [Just (SomeValue (ValueBigInt i))]) -> Just (num (fromInteger i))
  (FixParseBigInt, [Just (SomeValue (ValueString s))]) ->
    SomeValue . ValueBigInt <$> parseBigIntString (T.unpack s)
  _ -> Nothing
 where
  num = SomeValue . ValueNumber

-- | Fold @==@ on same-family literals and on literal frozen records.
eqFold :: Ir -> Ir -> Maybe Bool
eqFold (Ir (NLit (SomeValue a))) (Ir (NLit (SomeValue b)))
  | notFn a && notFn b = sameFamilyEq a b
 where
  notFn = \case ValueFunction _ -> False; _ -> True
eqFold (Ir (NFrozen as)) (Ir (NFrozen bs)) = do
  ra <- mapM irField as
  rb <- mapM irField bs
  recordEq ra rb
 where
  irField (IrField kind k (Ir (NLit v)))
    | kind == FPlain || kind == FExtra = Just (RF (kind == FExtra) k v)
  irField _ = Nothing
eqFold _ _ = Nothing

sameFamilyEq :: Value u -> Value v -> Maybe Bool
sameFamilyEq a b = case (a, b) of
  (ValueNumber x, ValueNumber y) -> Just (x == y)
  (ValueBigInt x, ValueBigInt y) -> Just (x == y)
  (ValueString x, ValueString y) -> Just (x == y)
  (ValueBool x, ValueBool y) -> Just (x == y)
  (ValueUnit, ValueUnit) -> Just True
  (ValueArray xs, ValueArray ys)
    | length xs /= length ys -> Just False
    | otherwise -> allM (zipWith sameFamilyEq xs ys)
  (ValueOption (Just x), ValueOption (Just y)) -> sameFamilyEq x y
  (ValueOption x, ValueOption y) -> Just (null x && null y)
  (ValueResult (Left x), ValueResult (Left y)) -> sameFamilyEq x y
  (ValueResult (Right x), ValueResult (Right y)) -> sameFamilyEq x y
  (ValueResult _, ValueResult _) -> Just False
  (ValueRegex x, ValueRegex y) -> Just (x == y)
  (ValueUint8Array x, ValueUint8Array y) -> Just (x == y)
  (ValueFrozen xs, ValueFrozen ys) -> recordEq (map valueField xs) (map valueField ys)
  (ValueFunction _, ValueFunction _) ->
    error "JShark.Compiler.Ir: functions cannot be compared for equality"
  _ -> Nothing
 where
  -- Stop at the first 'False'; an unknown pair before it blocks the fold.
  allM = \case
    [] -> Just True
    Just True : rest -> allM rest
    r : _ -> r

sameFamilyOrd :: Value u -> Value v -> Maybe Ordering
sameFamilyOrd a b = case (a, b) of
  (ValueNumber x, ValueNumber y) -> Just (compare x y)
  (ValueBigInt x, ValueBigInt y) -> Just (compare x y)
  (ValueString x, ValueString y) -> Just (compare x y)
  (ValueBool x, ValueBool y) -> Just (compare x y)
  _ -> Nothing

-- | A literal record field: extra flag, JS name, value.
data RF = RF !Bool !Text !SomeValue

valueField :: FieldLit Value r -> RF
valueField = \case
  FieldLit @k (Literal v) -> RF False (key (Proxy @k)) (SomeValue v)
  FieldLitExtra @k (Literal v) -> RF True (key (Proxy @k)) (SomeValue v)
  _ -> error "JShark.Compiler.Ir.valueField: unfrozen frozen-literal field"
 where
  key p = T.pack (symbolVal p)

-- | Last-wins record equality mirroring the evaluator's frozen equality.
recordEq :: [RF] -> [RF] -> Maybe Bool
recordEq as bs
  | length las /= length lbs = Just False
  | otherwise = Just (all (\fa -> any (rfEq fa) lbs) las)
 where
  las = keepLastByKey (\(RF _ k _) -> k) as
  lbs = keepLastByKey (\(RF _ k _) -> k) bs
  rfEq (RF xa ka (SomeValue va)) (RF xb kb (SomeValue vb)) =
    xa == xb && ka == kb && fromMaybe False (sameFamilyEq va vb)
