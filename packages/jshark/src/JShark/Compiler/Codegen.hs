{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Optimized IR to JavaScript.
--
-- 1. /Number/: pack the tree in post-order, wrapping effects that sit in
--    expression position ('NEmbed') and expressions in effect position
--    ('NLift'), then fold @lit op lit@ arithmetic the tree optimizer left.
-- 2. /Plan/: a pre-order walk names every binder (source hints under
--    'esSourceNames', otherwise @n0@, @n1@, …) and resolves variables.
-- 3. /Emit/: nodes emit leaves-first, by height then number, each from its
--    children's 'Code'. Temporaries and the shared preamble (runtime shims,
--    hoisted @$name@ helpers) are allocated in that order.
--
-- Internal to the JShark compiler; its API may change between 0.x releases.
module JShark.Compiler.Codegen
  ( EmitStyle (..)
  , minifiedStyle
  , idiomaticStyle
  , pureProgram
  , effectfulProgram
  , pureAST
  , pureASTWith
  , effectfulAST
  , effectfulASTWith
  )
where

import Control.Monad (replicateM)
import Control.Monad.State.Strict (State, modify', runState, state)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Char as Char
import Data.Foldable (toList)
import qualified Data.IntMap.Strict as IM
import Data.List (intersperse, sortOn)
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import JShark.Api.Prim (fixedBinaryJS, fixedTernaryJS, fixedUnaryJS, math1Name, math2Name)
import JShark.Api.Types
import JShark.Compiler.Emit
import JShark.Compiler.Evaluate (bigOpJS, jsBigIntLit, jsQuote, jsUint8ArrayLit, jsUint8ClampedArrayLit)
import JShark.Compiler.Ir
import JShark.Compiler.JsShim
import JShark.Compiler.Lower (lowerOptEffectIrWith, lowerOptExprIr)

-- | Codegen presentation. Syntax flags are safe for minified output;
-- structure flags ('esSourceNames', 'esKeepLets') are for readable output.
data EmitStyle = EmitStyle
  { esIntLiterals :: !Bool
  , esBareKeys :: !Bool
  , esSourceNames :: !Bool
  , esKeepLets :: !Bool
  }
  deriving (Eq, Show)

minifiedStyle, idiomaticStyle :: EmitStyle
minifiedStyle = EmitStyle True True False False
idiomaticStyle = EmitStyle True True True True

-- | Compile a closed pure expression to a JavaScript IIFE.
pureProgram :: ClosedExpr u -> JS
pureProgram e = renderIIFE (codegen minifiedStyle (fst (lowerOptExprIr False e)))

-- | Compile a closed effectful program to a JavaScript IIFE.
effectfulProgram :: ClosedEffect u -> JS
effectfulProgram e = renderIIFE (codegen minifiedStyle (fst (lowerOptEffectIrWith False e)))

pureAST :: ClosedExpr u -> JS
pureAST = pureASTWith idiomaticStyle

pureASTWith :: EmitStyle -> ClosedExpr u -> JS
pureASTWith st e = renderSnippet (codegen st (fst (lowerOptExprIr (esKeepLets st) e)))

effectfulAST :: ClosedEffect u -> JS
effectfulAST = effectfulASTWith idiomaticStyle

effectfulASTWith :: EmitStyle -> ClosedEffect u -> JS
effectfulASTWith st e = renderSnippet (codegen st (fst (lowerOptEffectIrWith (esKeepLets st) e)))

-- | Wrap preamble, declarations, and result in an IIFE so a minifier treats
-- the result as live.
renderIIFE :: (Preamble, Code) -> JS
renderIIFE (p, MkCode ds ref _) =
  let
    body = renderPreambleStyled p $$ fromMaybe mempty ds
    stmts = maybe body (\r -> body $$ (("return" <+> r) <> semi)) ref
   in
    "(() => {" <> iifeBody stmts <> "})()"

renderSnippet :: (Preamble, Code) -> JS
renderSnippet (p, MkCode a b _) = renderPreambleStyled p $$ fromMaybe mempty a $$ fromMaybe mempty b

-- Code ------------------------------------------------------------------------

-- | Declarations to emit first, the value reference, and whether the
-- reference is an effect (so it must still run as a statement).
data Code = MkCode
  { codeDecl :: !(Maybe JS)
  , codeRef :: !(Maybe JS)
  , _codeFX :: !Bool
  }

pattern Code :: JS -> JS -> Code
pattern Code d r <- MkCode (fromMaybe mempty -> d) (fromMaybe mempty -> r) _
  where
    Code d r = MkCode (nonEmpty d) (nonEmpty r) False

{-# COMPLETE Code #-}

fxCode :: JS -> JS -> Code
fxCode d r = MkCode (nonEmpty d) (nonEmpty r) True

decls :: [Code] -> JS
decls cs = vcat (mapMaybe codeDecl cs)

-- | A dropped unit ref would shorten an array literal, so print it.
refs :: [Code] -> JS
refs cs = hcat (intersperse ", " (map (fromMaybe "undefined" . codeRef) cs))

-- Numbering -------------------------------------------------------------------

-- | A numbered node. The annotation holds the names the plan chose for the
-- node's binders (or a variable's resolved name).
data P = P {pId :: !Int, pAnn :: [Text], pN :: N P}

data Pos = PE | PX | PRaw

-- | Where each child is packed: expression, effect, or as is.
positions :: N r -> N (Pos, r)
positions n = case n of
  NLetRec t r b -> NLetRec t (PRaw, r) (PRaw, b)
  NLam t i b -> NLam t i (PRaw, b)
  NLift x -> NLift (PE, x)
  NFFI f as -> NFFI f (map ((,) PRaw) as)
  NCall x m as -> NCall (PX, x) m (map ((,) PRaw) as)
  NFor s e t b -> NFor (PE, s) (PE, e) t (PX, b)
  NU8Set a b c -> NU8Set (PE, a) (PE, b) (PE, c)
  NU8Fill a b -> NU8Fill (PE, a) (PE, b)
  NOptCaseE o x t s -> NOptCaseE (PE, o) (PX, x) t (PX, s)
  NResCaseE o te e tk k -> NResCaseE (PE, o) te (PX, e) tk (PX, k)
  NStrCase s arms d -> NStrCase (PE, s) [(k, (PX, e)) | (k, e) <- arms] (PX, d)
  NThrow x -> NThrow (PE, x)
  NDelete o k -> NDelete (PX, o) (PE, k)
  NFrozen fs -> NFrozen (map field fs)
  NObjLit fs -> NObjLit (map field fs)
  _ -> fmap ((,) (if isEffectNode n then PX else PE)) n
 where
  field (IrField k name c) = IrField k name (if k == FEff || k == FExtraEff then PX else PE, c)

number :: Ir -> P
number root = fst (runState (at PE root) 0)
 where
  at :: Pos -> Ir -> State Int P
  at pos ir@(Ir n) = case pos of
    PE
      | NLift x <- n -> at PE x
      | isEffectNode n -> raw ir >>= node . NEmbed
    PX | not (isEffectNode n) -> raw ir >>= node . NLift
    _ -> raw ir
  raw :: Ir -> State Int P
  raw (Ir n) = traverse (uncurry at) (positions n) >>= node
  node :: N P -> State Int P
  node n = state (\i -> (P i [] n, i + 1))

-- | Fold @lit + lit@, @lit - lit@, @lit * lit@ bottom-up: let-inlining can
-- create them after the tree optimizer visited the node.
foldArith :: P -> P
foldArith (P i a n) = P i a $ case fmap foldArith n of
  NK2 op (pN -> LitV (ValueNumber x)) (pN -> LitV (ValueNumber y))
    | Just f <- arith op -> NLit (SomeValue (ValueNumber (f x y)))
  n' -> n'
 where
  arith = \case
    OPlus -> Just (+)
    OMinus -> Just (-)
    OTimes -> Just (*)
    _ -> Nothing

-- | Leaves first: by height, then by number.
layered :: P -> [P]
layered root = map snd (sortOn fst (snd (go root [])))
 where
  go p acc =
    let
      (hs, acc') = foldr (\c (hs', a) -> let (hc, a') = go c a in (hc : hs', a')) ([], acc) (toList (pN p))
      h = if null hs then 0 else 1 + maximum hs
     in
      (h :: Int, ((h, pId p), p) : acc')

-- Plan ------------------------------------------------------------------------

data PlanS = PlanS !Int [Set Text]

nName :: Int -> Text
nName n = "n" <> T.pack (show n)

-- | Name a binder. A source hint is used once per JS function scope.
alloc :: EmitStyle -> Maybe Text -> State PlanS Text
alloc st hint = state $ \(PlanS n sc) ->
  let
    top = fromMaybe S.empty (listToMaybe sc)
    name = case hint of
      Just t | t `S.notMember` top, esSourceNames st, jsSafeBinder t -> t
      _ -> nName n
    sc' = case sc of
      x : rest -> S.insert name x : rest
      [] -> [S.singleton name]
   in
    (name, PlanS (n + 1) sc')

-- | Plan inside a fresh JS function scope.
inScope :: State PlanS a -> State PlanS a
inScope m = modify' push *> m <* modify' pop
 where
  push (PlanS n sc) = PlanS n (S.empty : sc)
  pop (PlanS n sc) = PlanS n (case sc of _ : rest@(_ : _) -> rest; _ -> sc)

plan :: EmitStyle -> IM.IntMap Text -> P -> State PlanS P
plan st env (P i _ n) = case n of
  NVar t -> ret (toList (IM.lookup t env)) n
  NLet t h x b -> do
    x' <- go x
    v <- new h
    b' <- goWith [(t, v)] b
    ret [v] (NLet t h x' b')
  NLetRec t r b -> new Nothing >>= \v -> ret [v] =<< (NLetRec t <$> goWith [(t, v)] r <*> goWith [(t, v)] b)
  NBindRec t r b -> new Nothing >>= \v -> ret [v] =<< (NBindRec t <$> goWith [(t, v)] r <*> goWith [(t, v)] b)
  NLam t info b -> inScope $ new (lamParam info) >>= \v -> ret [v] . NLam t info =<< goWith [(t, v)] b
  NLamE t b -> inScope $ new Nothing >>= \v -> ret [v] . NLamE t =<< goWith [(t, v)] b
  NOptCase o x t s -> optCase (\o' x' s' -> NOptCase o' x' t s') o x t s
  NOptCaseE o x t s -> optCase (\o' x' s' -> NOptCaseE o' x' t s') o x t s
  NResCase o te e tk k -> resCase (\o' e' k' -> NResCase o' te e' tk k') o te e tk k
  NResCaseE o te e tk k -> resCase (\o' e' k' -> NResCaseE o' te e' tk k') o te e tk k
  NFnLit ts hs b -> inScope $ do
    vs <- mapM new hs
    ret vs . NFnLit ts hs =<< goWith (zip ts vs) b
  NMeth m a z ts b -> do
    a' <- go a
    z' <- traverse go z
    inScope $ do
      let k = if m == MFrom then 2 else length ts
      vs <- drop (k - length ts) <$> replicateM k (new Nothing)
      ret vs . NMeth m a' z' ts =<< goWith (zip ts vs) b
  NBind t h x b
    | P _ _ (NLift (P _ _ (NVar j))) <- b, j == t -> go x >>= \x' -> ret [] . NBind t h x' =<< go b
    | otherwise -> do
        x' <- go x
        v <- new h
        ret [v] . NBind t h x' =<< goWith [(t, v)] b
  NFor s e t b -> do
    s' <- go s
    e' <- go e
    v <- new Nothing
    ret [v] . NFor s' e' t =<< goWith [(t, v)] b
  NTry a t k -> do
    a' <- go a
    v <- new Nothing
    ret [v] . NTry a' t =<< goWith [(t, v)] k
  NStrCase s arms d -> do
    s' <- go s
    v <- new Nothing
    ret [v] =<< (NStrCase s' <$> traverse (traverse go) arms <*> go d)
  _ -> ret [] =<< traverse go n
 where
  go = plan st env
  goWith binds = plan st (foldr (uncurry IM.insert) env binds)
  new = alloc st
  ret ann n' = pure (P i ann n')
  optCase mk o x t s = do
    o' <- go o
    v <- new Nothing
    x' <- go x
    ret [v] . mk o' x' =<< goWith [(t, v)] s
  -- The result name, then the payload name shared by both arms.
  resCase mk o te e tk k = do
    o' <- go o
    r <- new Nothing
    u <- new Nothing
    e' <- goWith [(te, u)] e
    k' <- goWith [(tk, u), (te, u)] k
    ret [r, u] (mk o' e' k')

-- Emit ------------------------------------------------------------------------

-- | Emit state: the next temporary number and the preamble.
data ES = ES !Int !Preamble

codegen :: EmitStyle -> Ir -> (Preamble, Code)
codegen st ir =
  let
    (root, PlanS next _) = runState (plan st IM.empty (foldArith (number ir))) (PlanS 0 [S.empty])
    step (!table, !es) p =
      let (es', c) = emit st (table IM.!) es p in (IM.insert (pId p) c table, es')
    (tableF, ES _ pre) = foldl' step (IM.empty, ES next emptyPreamble) (layered root)
   in
    (pre, tableF IM.! pId root)

tmp :: ES -> (Text, ES)
tmp (ES n p) = (nName n, ES (n + 1) p)

shim :: Builtin -> [JS] -> ES -> (ES, JS)
shim b args (ES n p) = let (p', js) = useShim b args p in (ES n p', js)

emit :: EmitStyle -> (Int -> Code) -> ES -> P -> (ES, Code)
emit st table s0 (P _ ann n) = case n of
  NLit (SomeValue v) -> same (renderLit st v)
  NVar _ -> same (Code mempty (maybe mempty jsText (listToMaybe ann)))
  NEmbed e -> same (c e)
  NLift e -> same (c e)
  NLet t _ x b
    | P _ _ (NVar j) <- b, j == t -> same (c x)
    | MkCode xd xr _ <- c x, yc <- c b ->
        same (keepRef (fromMaybe mempty xd $$ constBind bound (fromMaybe mempty xr) $$ fromMaybe mempty (codeDecl yc)) yc)
  NLetRec _ r b | MkCode rd rr _ <- c r, bc <- c b -> same (keepRef (recBindStmt bound rd rr $$ fromMaybe mempty (codeDecl bc)) bc)
  NLam _ info b -> case lamTag info of
    Just tag ->
      let
        (params, body) = spine [bound] b
        MkCode d r _ = c body
        helper = hoistTagName tag
        src = TE.decodeUtf8 (renderJS (renderFn params d r))
        ES k p = s0
       in
        (ES k (insertHoisted helper src p), Code mempty (jsText helper))
    Nothing | MkCode d r _ <- c b -> same (Code mempty (renderFn [jsText bound] d r))
  NLamE _ b | MkCode d r _ <- c b -> same (Code mempty (renderFn [jsText bound] d r))
  NApp f x ->
    let (hd, args) = collect f [x]
     in if length args > 1 && callArity hd == length args
          then
            let Code fd fr = c hd; cs = map c args
             in same (Code (foldl' ($$) fd [d | Code d _ <- cs]) (parens fr <> parens (hcat (intersperse ", " [r | Code _ r <- cs]))))
          else let Code fd fr = c f; Code xd xr = c x in same (Code (fd $$ xd) (parens fr <> parens xr))
  NAppE f x | Code fd fr <- c f, Code xd xr <- c x -> same (fxCode (fd $$ xd) (parens fr <> parens xr))
  NIf cnd t e -> ifExpr False cnd t e
  NIfE cnd t e
    | isUnitE t && isUnitE e
    , MkCode cd cr _ <- c cnd
    , MkCode td tr _ <- c t
    , MkCode ed er _ <- c e ->
        same (MkCode (Just (fromMaybe mempty cd $$ ifElseStmt (fromMaybe mempty cr) td tr ed er)) Nothing False)
    | otherwise -> ifExpr True cnd t e
  NOptCase o x _ s ->
    let
      MkCode od orf _ = c o
      (ov, s1) = tmp s0
      MkCode nd nr _ = c x
      MkCode sd sr _ = c s
      (rv, s2) = tmp s1
      body =
        fromMaybe mempty od
          $$ constBind ov (fromMaybe "null" orf)
          $$ letResult rv
          $$ ifAssignOrStmt (Just rv) (jsText ov <+> ".some") (Just (constBind bound (jsText ov <> ".value") $$ fromMaybe mempty sd)) sr nd nr
     in
      (s2, MkCode (Just body) (Just (jsText rv)) False)
  NOptCaseE o x _ s ->
    let
      MkCode od orf _ = c o
      (ov, s1) = tmp s0
      pre = fromMaybe mempty od $$ constBind ov (fromMaybe "null" orf) $$ constBind bound (jsText ov <> ".value")
      MkCode nd nr _ = c x
      MkCode sd sr _ = c s
     in
      branching (isUnitE x && isUnitE s) s1 pre $ \mr -> ifAssignOrStmt mr (jsText ov <+> ".some") sd sr nd nr
  NResOk x | MkCode d r _ <- c x -> same (MkCode d (Just (resultObject True r)) False)
  NResErr x | MkCode d r _ <- c x -> same (MkCode d (Just (resultObject False r)) False)
  NResCase o _ e _ k ->
    let
      (obj, s1, pre) = resPrelude o
      MkCode ed er _ = c e
      MkCode od orf _ = c k
      cond = jsText obj <> ".ok"
     in
      if isNothing ed && isNothing od
        then (s1, MkCode (Just pre) (Just (parens (cond <+> "?" <+> undef orf <+> ":" <+> undef er))) False)
        else
          let (rv, s2) = tmp s1
           in (s2, MkCode (Just (pre $$ letResult rv $$ ifAssignOrStmt (Just rv) cond od orf ed er)) (Just (jsText rv)) False)
  NResCaseE o _ e _ k ->
    let
      (obj, s1, pre) = resPrelude o
      MkCode ed er _ = c e
      MkCode od orf _ = c k
      cond = jsText obj <> ".ok"
     in
      if isUnitE e && isUnitE k
        then (s1, Code (pre $$ ifElseStmt cond od orf ed er) mempty)
        else
          let
            rv = bound
            stmt =
              pre
                $$ letResult rv
                $$ ifElseStmt cond (Just (fromMaybe mempty od $$ assignResult rv orf)) Nothing (Just (fromMaybe mempty ed $$ assignResult rv er)) Nothing
           in
            (s1, Code stmt (jsText rv))
  NIndex a i | Code ad ar <- c a, Code idd ir <- c i -> let (s1, call) = shim CheckedIndex [ar, ir] s0 in (s1, Code (ad $$ idd) call)
  NU8Index a i | Code ad ar <- c a, Code idd ir <- c i -> same (Code (ad $$ idd) (ar <> brackets ir))
  NError m | Code d r <- c m -> same (Code d ("(function(){throw new Error(" <> r <> ");}())"))
  NFixed (SomeFixedOp op) xs -> fixed op xs
  NFnLit _ _ b | Code d r <- c b -> same (Code mempty (renderFn (map jsText ann) (nonEmpty d) (Just r)))
  NNullable x | Code d r <- c x -> same (Code d ("((v) => v == null ? {some: false} : {some: true, value: v})" <> parens r))
  NFrozen fs -> same (objectLit fs)
  NObjLit fs -> same (objectLit fs)
  NGetField k o | Code d r <- c o -> same (Code d (jsDotOrBracket r k))
  NUGet o k | Code d r <- c o -> same (Code d (jsDotOrBracket r k))
  NK2 OAnd x y -> shortCircuit True x y
  NK2 OOr x y -> shortCircuit False x y
  NK2 (OEq True) x y -> valueEq id x y
  NK2 (ONEq True) x y -> valueEq (\js -> "!" <> parens js) x y
  NK2 op x y | Code xd xr <- c x, Code yd yr <- c y -> same (Code (xd $$ yd) (operand x xr <+> jsText (op2JS op) <+> operand y yr))
  NK1 op x | Code d r <- c x -> same . Code d $ case op of
    OShow -> "String" <> parens r
    OTypeOf -> "typeof" <+> r
    _ -> "-" <> parens r
  NMeth m a z _ b ->
    let
      Code ad ar = c a
      Code bd br = c b
      cb params = renderFn params (nonEmpty bd) (Just br)
      recv = operand a ar
     in
      same $ case (m, z) of
        (MMap, _) -> Code ad (recv <> ".map" <> parens (cb (map jsText ann)))
        (MFilter, _) -> Code ad (recv <> ".filter" <> parens (cb (map jsText ann)))
        (MToSorted, _) -> Code ad (recv <> ".toSorted" <> parens (cb (map jsText ann)))
        (MFrom, _) -> Code ad ("Array.from({length: " <> ar <> "}, " <> cb ("_" : map jsText ann) <> ")")
        (_, Just zc) | Code zd zr <- c zc ->
          let method = if m == MReduce then ".reduce" else ".reduceRight"
           in Code (ad $$ zd) (recv <> method <> parens (cb (map jsText ann) <> ", " <> zr))
        _ -> error "JShark.Compiler.Codegen: reduce without a seed"
  NFFI form args -> let cs = map c args in same (fxCode (decls cs) (renderFFIInvoke form (refs cs)))
  NUObj t -> same (Code mempty (jsText t))
  NUSet x y | Code xd xr <- c x, Code yd yr <- c y -> same (fxCode (xd $$ yd) (xr <> " = " <> yr))
  NCall o m args
    | Code od orf <- c o
    , cs <- map c args ->
        same (fxCode (od $$ decls cs) (orf <> "." <> jsText m <> parens (refs cs)))
  NBind t _ x b
    | P _ _ (NLift (P _ _ (NVar j))) <- b, j == t -> same (c x)
    | MkCode xd xr xfx <- c x
    , MkCode yd yr yfx <- c b ->
        same $ case xr of
          Nothing -> MkCode (Just (stmtOf xd xr xfx $$ fromMaybe mempty yd)) yr yfx
          Just r -> MkCode (Just (fromMaybe mempty xd $$ constBind bound r $$ fromMaybe mempty yd)) yr yfx
  NThen x y
    | MkCode xd xr xfx <- c x
    , MkCode yd yr yfx <- c y ->
        same (MkCode (Just (stmtOf xd xr xfx $$ fromMaybe mempty yd)) yr yfx)
  NBindRec _ r b
    | MkCode rd rr _ <- c r
    , MkCode bd br bfx <- c b ->
        same (MkCode (Just (recBindStmt bound rd rr $$ fromMaybe mempty bd)) br bfx)
  NWhile cnd b
    | MkCode cd cr _ <- c cnd
    , MkCode bd br _ <- c b ->
        let
          body = asStmt bd br
          cjs = fromMaybe "false" cr
         in
          same . (\js -> MkCode (Just js) Nothing False) $ case cd of
            Nothing -> "while" <+> parens cjs <+> blockBody body
            Just d -> "while" <+> parens "true" <+> blockBody ((d $$ ("if" <+> parens ("!" <> parens cjs) <+> blockBody "break;")) $$ body)
  NFor a e _ b
    | MkCode ad ar _ <- c a
    , MkCode ed er _ <- c e
    , MkCode bd br _ <- c b ->
        let
          v = jsText bound
          hd = hcat [("let" <+> v <+> "=" <+> fromMaybe mempty ar), ";", " ", (v <+> "<" <+> fromMaybe mempty er), ";", " ", v <> "++"]
         in
          same (MkCode (Just (fromMaybe mempty ad $$ fromMaybe mempty ed $$ ("for" <+> parens hd <+> blockBody (asStmt bd br)))) Nothing False)
  NU8Set a i v
    | Code ad ar <- c a
    , Code idd ir <- c i
    , Code vd vr <- c v ->
        same (Code (ad $$ idd $$ vd $$ (((ar <> brackets ir) <+> "=" <+> vr) <> semi)) mempty)
  NU8Fill a v | Code ad ar <- c a, Code vd vr <- c v -> same (Code (ad $$ vd $$ ((ar <> ".fill" <> parens vr) <> semi)) mempty)
  NStrCase o arms d ->
    let
      unit = all (isUnitE . snd) arms && isUnitE d
      Code od orf = c o
      rv = bound
      arm e
        | MkCode md mr _ <- c e =
            if unit then asStmt md mr else fromMaybe mempty md $$ assignResult rv mr
      cases = ["case" <+> (jsQuote k <> colon) <+> blockBody (arm e <+> ("break" <> semi)) | (k, e) <- arms]
      sw = "switch" <+> parens orf <+> blockBody (vcat (cases ++ ["default:" <+> blockBody (arm d)]))
      pre = if unit then od else od $$ letResult rv
     in
      same (MkCode (Just (pre $$ sw)) (if unit then Nothing else Just (jsText rv)) False)
  NThrow x | Code d r <- c x -> same (Code (d $$ (("throw" <+> r) <> semi)) mempty)
  NTry a _ k
    | MkCode ad ar _ <- c a
    , MkCode kd kr _ <- c k ->
        branching (isUnitE a && isUnitE k) s0 mempty $ \mr -> tryCatchStmt mr (jsText bound) ad ar kd kr
  NDelete o k | Code od orf <- c o, Code kd kr <- c k -> same (fxCode (od $$ kd) (("delete" <+> orf) <> brackets kr))
  NArray es -> let cs = map c es in same (Code (decls cs) (brackets (refs cs)))
 where
  c = table . pId
  same x = (s0, x)
  bound = fromMaybe "" (listToMaybe ann)
  undef = fromMaybe "undefined"

  -- The effect form rebuilds its declarations with 'Code', dropping an
  -- empty one.
  ifExpr eff cnd t e =
    let
      MkCode cd cr _ = c cnd
      MkCode td tr tfx = c t
      MkCode ed er efx = c e
      cjs = fromMaybe mempty cr
      cd' = if eff then nonEmpty (fromMaybe mempty cd) else cd
     in
      if isNothing td && isNothing ed && not tfx && not efx
        then same (MkCode cd' (Just (parens (cjs <+> "?" <+> undef tr <+> ":" <+> undef er))) False)
        else
          let (rv, s1) = tmp s0
           in (s1, MkCode (Just (fromMaybe mempty cd $$ letResult rv $$ ifAssignOrStmt (Just rv) cjs td tr ed er)) (Just (jsText rv)) False)

  -- Unit arms: prelude and statement. Value arms also declare the result.
  branching unit s1 pre k
    | unit = (s1, MkCode (Just (pre $$ k Nothing)) Nothing False)
    | otherwise =
        let (rv, s2) = tmp s1
         in (s2, MkCode (Just (pre $$ letResult rv $$ k (Just rv))) (Just (jsText rv)) False)

  resPrelude o =
    let
      MkCode od orf _ = c o
      (obj, s1) = tmp s0
      unw = fromMaybe "" (listToMaybe (drop 1 ann))
     in
      (obj, s1, fromMaybe mempty od $$ constBind obj (fromMaybe mempty orf) $$ constBind unw (jsText obj <> ".value"))

  -- @&&@ \/ @||@ evaluate the right operand only when selected, so its
  -- declarations are guarded rather than hoisted.
  shortCircuit isAnd x y =
    let
      MkCode xd xr _ = c x
      xjs = operand x (undef xr)
     in
      case c y of
        MkCode Nothing yr _ ->
          same (MkCode xd (Just (xjs <+> (if isAnd then "&&" else "||") <+> operand y (undef yr))) False)
        MkCode (Just yd) yr _ ->
          let
            (rv, s1) = tmp s0
            v = jsText rv
            cond = if isAnd then v else "!" <> parens v
            guard = yd $$ ((v <+> "=" <+> undef yr) <> semi)
            xInit = ("let" <+> v <+> "=" <+> xjs) <> semi
           in
            (s1, MkCode (Just (fromMaybe mempty xd $$ xInit $$ ("if" <+> parens cond <+> blockBody guard))) (Just v) False)

  valueEq wrap x y
    | Code xd xr <- c x
    , Code yd yr <- c y =
        let (s1, js) = shim ValueEq [operand x xr, operand y yr] s0 in (s1, Code (xd $$ yd) (wrap js))

  fixed :: FixedOp a b cc u -> [P] -> (ES, Code)
  fixed op xs = case (op, xs, map c xs) of
    (_, _, [Code d r]) | Just nm <- math1Name op -> same (Code d ("Math." <> jsText nm <> parens r))
    (_, _, [Code d r, Code d' r']) | Just nm <- math2Name op -> same (Code (d $$ d') ("Math." <> jsText nm <> parens (r <> ", " <> r')))
    (_, [x], [Code d r]) -> same (Code d (fixedUnaryJS op (operand x r)))
    (FixGroupBy, [x, _], [Code d r, Code d' r']) ->
      let (s1, call) = shim GroupBy [operand x r, r'] s0 in (s1, Code (d $$ d') call)
    (_, [x, _], [Code d r, Code d' r']) -> same (Code (d $$ d') (fixedBinaryJS op (operand x r) r'))
    (_, [x, _, _], [Code d r, Code d' r', Code d'' r'']) -> same (Code (d $$ d' $$ d'') (fixedTernaryJS op (operand x r) r' r''))
    _ -> error "JShark.Compiler.Codegen: unexpected fixed arity"

  objectLit fs =
    let parts = [(d, (jsPropKey st k <> ":") <+> r) | IrField _ k e <- fs, Code d r <- [c e]]
     in Code (mconcat (mapMaybe (nonEmpty . fst) parts)) (braces (hcat (intersperse ", " (map snd parts))))

  -- A hoisted lambda absorbs directly nested untagged lambdas as parameters.
  spine params b = case pN b of
    NLam _ info b' | isNothing (lamTag info) -> spine (params ++ take 1 (pAnn b)) b'
    _ -> (map jsText params, b)

  collect f args = case pN f of
    NApp f' x' -> collect f' (x' : args)
    _ -> (f, args)

  callArity p = case pN p of
    NLam _ info b | isJust (lamTag info) -> 1 + untagged b
    _ -> 0
  untagged p = case pN p of
    NLam _ info b | isNothing (lamTag info) -> 1 + untagged b
    _ -> 0

  operand p d = if isSimple p then d else parens d

keepRef :: JS -> Code -> Code
keepRef d (MkCode _ r f) = MkCode (nonEmpty d) r f

-- | A rendered effect as a statement: a pure value with declarations keeps
-- only the declarations.
stmtOf :: Maybe JS -> Maybe JS -> Bool -> JS
stmtOf d r fx
  | isNothing r || (not fx && isJust d) = fromMaybe mempty d
  | otherwise = asStmt d r

isUnitE :: P -> Bool
isUnitE p = case pN p of
  NLift (pN -> LitV ValueUnit) -> True
  NThrow _ -> True
  NWhile _ b -> isUnitE b
  NFor _ _ _ b -> isUnitE b
  NThen _ y -> isUnitE y
  NBind _ _ _ y -> isUnitE y
  NBindRec _ _ y -> isUnitE y
  NIfE _ t e -> isUnitE t && isUnitE e
  NOptCaseE _ x _ s -> isUnitE x && isUnitE s
  NResCaseE _ _ e _ k -> isUnitE e && isUnitE k
  NStrCase _ arms d -> all (isUnitE . snd) arms && isUnitE d
  NTry a _ k -> isUnitE a && isUnitE k
  _ -> False

-- | Operands that never need parentheses.
isSimple :: P -> Bool
isSimple p = case pN p of
  NLit _ -> True
  NVar _ -> True
  NEmbed e -> simpleEffect e
  NK1 {} -> True
  NFixed {} -> True
  NFnLit {} -> True
  NIndex {} -> True
  NU8Index {} -> True
  NNullable x -> isSimple x
  NFrozen {} -> True
  NGetField {} -> True
  _ -> False
 where
  simpleEffect q = case pN q of
    NLift e -> isSimple e
    NFFI {} -> True
    NCall {} -> True
    NUObj {} -> True
    NUGet {} -> True
    NArray es -> all simpleEffect es
    _ -> False

op2JS :: Op2 -> Text
op2JS = \case
  OConcat -> "+"
  OPlus -> "+"
  OMinus -> "-"
  OTimes -> "*"
  ODiv -> "/"
  ORem -> "%"
  OBitAnd -> "&"
  OBitOr -> "|"
  OBitXor -> "^"
  OShl -> "<<"
  OShr -> ">>"
  OUShr -> ">>>"
  OBig op -> bigOpJS op
  OEq _ -> "==="
  ONEq _ -> "!=="
  OGTh -> ">"
  OLTh -> "<"
  OGTEq -> ">="
  OLTEq -> "<="
  OAnd -> "&&"
  OOr -> "||"

renderLit :: EmitStyle -> Value u -> Code
renderLit st = \case
  ValueNumber d -> Code mempty (jsNumber st d)
  ValueBigInt i -> Code mempty (jsBigIntLit i)
  ValueArray xs -> let cs = map (renderLit st) xs in Code (decls cs) (brackets (refs cs))
  ValueString s -> Code mempty (jsQuote s)
  ValueUnit -> MkCode Nothing Nothing False
  ValueOption (Just x) | MkCode d r fx <- renderLit st x -> MkCode d (Just ("{some: true, value: " <> fromMaybe "undefined" r <> "}")) fx
  ValueOption Nothing -> Code mempty "{some: false}"
  ValueResult (Right x) | MkCode d r _ <- renderLit st x -> MkCode d (Just (resultObject True r)) False
  ValueResult (Left x) | MkCode d r _ <- renderLit st x -> MkCode d (Just (resultObject False r)) False
  ValueRegex s -> Code mempty ("new RegExp" <> parens (jsQuote s))
  ValueUint8Array ba -> Code mempty (jsUint8ArrayLit ba)
  ValueUint8ClampedArray ba -> Code mempty (jsUint8ClampedArrayLit ba)
  ValueBool b -> Code mempty (if b then "true" else "false")
  ValueFunction _ -> error "JShark.Compiler.Codegen: ValueFunction is eval-only"
  ValueFrozen {} -> error "JShark.Compiler.Codegen: ValueFrozen is eval-only"

resultObject :: Bool -> Maybe JS -> JS
resultObject isOk payload =
  braces ((("ok:" <+> (if isOk then "true" else "false")) <> ",") <+> ("value:" <+> fromMaybe "undefined" payload))

jsNumber :: EmitStyle -> Double -> JS
jsNumber st d
  | esIntLiterals st
  , not (isNaN d || isInfinite d)
  , -- @-0.0@ is a distinct JS value and must keep its sign.
    not (isNegativeZero d)
  , let i = round d :: Integer
  , fromInteger i == d
  , abs i <= 9007199254740991 =
      jsDecimal i
  | otherwise = jsDouble d

jsPropKey :: EmitStyle -> Text -> JS
jsPropKey st k
  | esBareKeys st && jsIdent k = jsText k
  | otherwise = dquotes (jsText k)

-- | @o.k@ for an identifier or a dotted identifier path, else @o["k"]@.
jsDotOrBracket :: JS -> Text -> JS
jsDotOrBracket obj k
  | jsIdent k = obj <> "." <> jsText k
  | (seg, rest) <- T.break (== '.') k
  , not (T.null rest)
  , jsIdent seg =
      jsDotOrBracket (jsDotOrBracket obj seg) (T.drop 1 rest)
  | otherwise = obj <> "[" <> dquotes (jsText k) <> "]"

jsIdent :: Text -> Bool
jsIdent t = case T.uncons t of
  Nothing -> False
  Just (x, xs) -> start x && T.all (\y -> start y || Char.isDigit y) xs
 where
  start x = Char.isAscii x && (Char.isLetter x || x == '_' || x == '$')

jsSafeBinder :: Text -> Bool
jsSafeBinder t = jsIdent t && t `S.notMember` reserved
 where
  reserved =
    S.fromList
      [ "break", "case", "catch", "class", "const", "continue", "debugger", "default", "delete"
      , "do", "else", "export", "extends", "false", "finally", "for", "function", "if", "import"
      , "in", "instanceof", "new", "null", "return", "super", "switch", "this", "throw", "true"
      , "try", "typeof", "var", "void", "while", "with", "yield", "enum", "await", "let"
      , "static", "implements", "interface", "package", "private", "protected", "public"
      ]

-- Statements ------------------------------------------------------------------

constBind :: Text -> JS -> JS
constBind v ref = ("const" <+> jsText v <+> "=" <+> ref) <> semi

recBindStmt :: Text -> Maybe JS -> Maybe JS -> JS
recBindStmt v d r = fromMaybe mempty d $$ constBind v (fromMaybe mempty r)

letResult :: Text -> JS
letResult v = ("let" <+> jsText v) <> semi

assignResult :: Text -> Maybe JS -> JS
assignResult v = maybe mempty (\r -> (jsText v <+> "=" <+> r) <> semi)

asStmt :: Maybe JS -> Maybe JS -> JS
asStmt d r = fromMaybe mempty d $$ maybe mempty (<> semi) r

ifElseStmt :: JS -> Maybe JS -> Maybe JS -> Maybe JS -> Maybe JS -> JS
ifElseStmt cnd td tr ed er
  | isNothing ed && isNothing er = ifThen
  | otherwise = ifThen $$ "else" <+> blockBody (asStmt ed er)
 where
  ifThen = "if" <+> parens cnd <+> blockBody (asStmt td tr)

ifAssignOrStmt :: Maybe Text -> JS -> Maybe JS -> Maybe JS -> Maybe JS -> Maybe JS -> JS
ifAssignOrStmt Nothing cnd td tr ed er = ifElseStmt cnd td tr ed er
ifAssignOrStmt (Just rv) cnd td tr ed er =
  "if" <+> parens cnd <+> blockBody (fromMaybe mempty td $$ assignResult rv tr)
    $$ "else" <+> blockBody (fromMaybe mempty ed $$ assignResult rv er)

tryCatchStmt :: Maybe Text -> JS -> Maybe JS -> Maybe JS -> Maybe JS -> Maybe JS -> JS
tryCatchStmt mr v ad ar bd br = case mr of
  Nothing -> "try" <+> blockBody (asStmt ad ar) $$ (catchHead <+> blockBody (asStmt bd br))
  Just rv ->
    "try" <+> blockBody (fromMaybe mempty ad $$ assignResult rv ar)
      $$ (catchHead <+> blockBody (fromMaybe mempty bd $$ assignResult rv br))
 where
  catchHead = "catch" <+> parens v

-- | Arrow function. Object-literal bodies are parenthesized.
renderFn :: [JS] -> Maybe JS -> Maybe JS -> JS
renderFn params d r = case (d, r) of
  (Nothing, Nothing) -> hd <+> blockBody mempty
  (Nothing, Just x) -> hd <+> arrowExpr x
  (Just ds, Nothing) -> hd <+> blockBody (ds $$ "return")
  (Just ds, Just x) -> hd <+> blockBody (ds $$ ("return" <+> arrowExpr x))
 where
  hd = (case params of [p] -> p; ps -> parens (hcat (intersperse ", " ps))) <+> "=>"
  arrowExpr x = if "{" `BS.isPrefixOf` BC.strip (renderJS x) then parens x else x

-- | Multi-parameter arrows need an extra pair of parentheses to be called.
renderFFIInvoke :: FFIForm -> JS -> JS
renderFFIInvoke fn args = case fn of
  FFILambda s -> parens (jsText s) <> parens args
  FFIExpr s -> jsText s
  FFICall s
    | "=>" `T.isInfixOf` s && not (wholeParens s) -> parens (jsText s) <> parens args
    | otherwise -> jsText s <> parens args
 where
  wholeParens t = case T.uncons t >>= \(o, rest) -> (,) o <$> T.unsnoc rest of
    Just ('(', (inner, ')')) -> balanced inner (0 :: Int)
    _ -> False
  balanced t depth = case T.uncons t of
    Nothing -> depth == 0
    Just ('(', rest) -> balanced rest (depth + 1)
    Just (')', rest) -> depth /= 0 && balanced rest (depth - 1)
    Just (_, rest) -> balanced rest depth
