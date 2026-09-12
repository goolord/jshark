{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Universe-indexed copies of the @base@ classes that apply to
-- 'Array', 'Option', 'Result', and 'Function', plus the @*@-kind
-- 'Semigroup' / 'Monoid' instances those classes need.
--
-- These are not the "Prelude" classes: @t :: Universe -> Universe@, and
-- methods map object-language values. Import qualified, or hide the
-- "Prelude" names. 'Semigroup' / 'Monoid' on @Expr f ('Array u)@ (and
-- friends) *are* the @base@ classes.
module JShark.Api.Classes
  ( -- * Functor
    Functor (..)
  , void

    -- * Applicative
  , Applicative (..)
  , Alternative (..)
  , liftA
  , liftA3
  , optional
  , guard

    -- * Monad
  , Monad (..)
  , MonadPlus (..)
  , MonadFail (..)
  , MonadZip (..)
  , MonadFix (..)
  , return
  , (>>)
  , (=<<)
  , (>=>)
  , (<=<)
  , ap
  , join
  , when
  , unless
  , mfilter
  , mapM
  , mapM_
  , forM
  , forM_
  , sequence
  , sequence_

    -- * Foldable
  , Foldable (..)
  , fold
  , concat
  , concatMap
  , and
  , or
  , any
  , all

    -- * Traversable
  , Traversable (..)
  , sequenceA
  , for
  , for_
  , traverse_

    -- * Bifunctor
  , Bifunctor (..)
  , first
  , second
  , Bifoldable (..)
  , Bitraversable (..)
  , bisequence

    -- * Category
  , Category (..)

    -- * Semigroup / Monoid (base instances on Expr)
  , (<>)
  , mempty
  )
where

import JShark.Api
  ( apply
  , emptyArray
  , err
  , if_
  , lambda
  , letRec
  , none
  , not_
  , ok
  , optionCase
  , resultCase
  , some
  , (.==)
  , (.||)
  )
import JShark.Api.Types
import qualified JShark.Array as A
import Prelude
  ( Bool (False, True)
  , const
  , mempty
  , ($)
  , (<>)
  )
import qualified Prelude as P

-- | Universe-indexed 'Prelude.Functor': maps a pure JS array, option,
-- result, or function value.
class Functor (t :: Universe -> Universe) where
  -- | Map an object-language function over a mapped value.
  fmap :: (Expr f a -> Expr f b) -> Expr f (t a) -> Expr f (t b)

  -- | Replace every element of a mapped value with a constant.
  (<$) :: Expr f a -> Expr f (t b) -> Expr f (t a)
  (<$) = fmap P.. const

instance Functor Array where
  fmap f xs = A.map xs f

instance Functor Option where
  fmap f o = optionCase o none (some P.. f)

instance Functor (Result e) where
  fmap f r = resultCase r err (ok P.. f)

instance Functor (Function r) where
  fmap f g = lambda (\x -> f (apply g x))

-- | Discard the result of a mapped value, yielding @unit@.
void :: Functor t => Expr f (t a) -> Expr f (t 'Unit)
void = (Literal ValueUnit <$)

-- | Universe-indexed 'Prelude.Applicative': applies wrapped object-language
-- functions to wrapped arguments.
class Functor t => Applicative t where
  -- | Lift an object-language value into the target universe.
  pure :: Expr f a -> Expr f (t a)

  -- | Lift a binary object-language function over two mapped values.
  liftA2 ::
    (Expr f a -> Expr f b -> Expr f c)
    -> Expr f (t a)
    -> Expr f (t b)
    -> Expr f (t c)

  -- | Apply a mapped function to a mapped argument.
  (<*>) :: Expr f (t (Function a b)) -> Expr f (t a) -> Expr f (t b)
  (<*>) = liftA2 apply

  -- | Sequence two mapped values, keeping the second.
  (*>) :: Expr f (t a) -> Expr f (t b) -> Expr f (t b)
  (*>) = liftA2 (const P.id)

  -- | Sequence two mapped values, keeping the first.
  (<*) :: Expr f (t a) -> Expr f (t b) -> Expr f (t a)
  (<*) = liftA2 const

instance Applicative Array where
  pure = A.singleton
  liftA2 f xs ys =
    A.reduce xs emptyArray $ \acc x ->
      A.concat acc (A.map ys (f x))

instance Applicative Option where
  pure = some
  liftA2 f o p =
    optionCase o none $ \x ->
      optionCase p none $ \y ->
        some (f x y)

instance Applicative (Result e) where
  pure = ok
  liftA2 f r s =
    resultCase r err $ \x ->
      resultCase s err $ \y ->
        ok (f x y)

instance Applicative (Function r) where
  pure x = lambda (\_ -> x)
  liftA2 f g h = lambda (\r -> f (apply g r) (apply h r))

-- | Like @base@ @liftA@: lift a unary function over a mapped value.
liftA :: Applicative t => (Expr f a -> Expr f b) -> Expr f (t a) -> Expr f (t b)
liftA f = liftA2 (const f) (pure (Literal ValueUnit))

-- | Like @base@ @liftA3@: lift a ternary function over three mapped values.
liftA3 ::
  Applicative t =>
  (Expr f a -> Expr f b -> Expr f c -> Expr f d)
  -> Expr f (t a)
  -> Expr f (t b)
  -> Expr f (t c)
  -> Expr f (t d)
liftA3 f x y z = liftA2 (\a b -> lambda (f a b)) x y <*> z

-- | Universe-indexed 'Prelude.Alternative': an empty value plus a choice.
class Applicative t => Alternative t where
  -- | The empty 'Array' or 'Option'.
  empty :: Expr f (t a)

  -- | Choose the first non-empty alternative.
  (<|>) :: Expr f (t a) -> Expr f (t a) -> Expr f (t a)

instance Alternative Array where
  empty = emptyArray
  (<|>) = A.concat

instance Alternative Option where
  empty = none
  o <|> d = optionCase o d (\_ -> o)

-- | Like @base@ @optional@. On 'Array' this is @map some xs <> [none]@ (same as @[]@).
optional ::
  Alternative t => Expr f (t a) -> Expr f (t (Option a))
optional x = fmap some x <|> pure none

-- | @unit@ when the condition holds, 'empty' otherwise.
guard :: Alternative t => Expr f 'Bool -> Expr f (t 'Unit)
guard c = if_ c (pure (Literal ValueUnit)) empty

-- | Universe-indexed 'Prelude.Monad': sequences object-language computations.
class Applicative t => Monad t where
  -- | Sequentially compose two object-language computations.
  (>>=) :: Expr f (t a) -> (Expr f a -> Expr f (t b)) -> Expr f (t b)

instance Monad Array where
  xs >>= k = A.reduce xs emptyArray (\acc x -> A.concat acc (k x))

instance Monad Option where
  o >>= k = optionCase o none k

instance Monad (Result e) where
  r >>= k = resultCase r err k

instance Monad (Function r) where
  g >>= k = lambda (\r -> apply (k (apply g r)) r)

-- | 'pure' under the 'Monad' name.
return :: Monad t => Expr f a -> Expr f (t a)
return = pure

-- | Sequence two computations, keeping the second.
(>>) :: Monad t => Expr f (t a) -> Expr f (t b) -> Expr f (t b)
(>>) = (*>)

-- | Flipped '(>>=)'.
(=<<) :: Monad t => (Expr f a -> Expr f (t b)) -> Expr f (t a) -> Expr f (t b)
(=<<) = P.flip (>>=)

-- | Left-to-right Kleisli composition.
(>=>) ::
  Monad t =>
  (Expr f a -> Expr f (t b))
  -> (Expr f b -> Expr f (t c))
  -> Expr f a
  -> Expr f (t c)
f >=> g = \x -> f x >>= g

-- | Right-to-left Kleisli composition.
(<=<) ::
  Monad t =>
  (Expr f b -> Expr f (t c))
  -> (Expr f a -> Expr f (t b))
  -> Expr f a
  -> Expr f (t c)
(<=<) = P.flip (>=>)

-- | @(<*>)@ via 'Monad'.
ap :: Monad t => Expr f (t (Function a b)) -> Expr f (t a) -> Expr f (t b)
ap = (<*>)

-- | Flatten a nested computation.
join :: Monad t => Expr f (t (t a)) -> Expr f (t a)
join m = m >>= P.id

-- | Run an action when the condition holds.
when :: Monad t => Expr f 'Bool -> Expr f (t 'Unit) -> Expr f (t 'Unit)
when c m = if_ c m (pure (Literal ValueUnit))

-- | Run an action when the condition fails.
unless :: Monad t => Expr f 'Bool -> Expr f (t 'Unit) -> Expr f (t 'Unit)
unless c = when (not_ c)

-- | Universe-indexed 'Control.Monad.MonadPlus': 'Alternative' plus 'Monad'.
class (Alternative t, Monad t) => MonadPlus t where
  -- | 'MonadPlus' identity, 'empty'.
  mzero :: Expr f (t a)
  mzero = empty

  -- | 'MonadPlus' choice, '(<|>)'.
  mplus :: Expr f (t a) -> Expr f (t a) -> Expr f (t a)
  mplus = (<|>)

instance MonadPlus Array

instance MonadPlus Option

-- | Universe-indexed 'Control.Monad.Fail.MonadFail'.
class Monad t => MonadFail t where
  -- | Fail with an object-language message.
  fail :: Expr f 'String -> Expr f (t a)

instance MonadFail Array where
  fail _ = empty

instance MonadFail Option where
  fail _ = empty

-- | Keep the elements of a computation that satisfy the predicate.
mfilter ::
  MonadPlus t => (Expr f a -> Expr f 'Bool) -> Expr f (t a) -> Expr f (t a)
mfilter p m = m >>= \x -> if_ (p x) (pure x) mzero

-- | Universe-indexed 'Control.Monad.Zip.MonadZip'.
class Monad t => MonadZip t where
  -- | Zip two mapped values with a binary object-language function.
  mzipWith ::
    (Expr f a -> Expr f b -> Expr f c)
    -> Expr f (t a)
    -> Expr f (t b)
    -> Expr f (t c)

instance MonadZip Array where
  mzipWith = A.zipWith

-- | Universe-indexed 'Control.Monad.Fix.MonadFix'.
class Monad t => MonadFix t where
  -- | The fixpoint combinator on object-language functions.
  mfix :: (Expr f a -> Expr f (t a)) -> Expr f (t a)

instance MonadFix (Function r) where
  mfix k = lambda (\r -> letRec (\a -> apply (k a) r) P.id)

-- | Universe-indexed 'Data.Foldable.Foldable'.
class Foldable (t :: Universe -> Universe) where
  -- | Right-fold a mapped value.
  foldr ::
    (Expr f a -> Expr f b -> Expr f b)
    -> Expr f b
    -> Expr f (t a)
    -> Expr f b

  -- | Left-fold a mapped value.
  foldl ::
    (Expr f b -> Expr f a -> Expr f b)
    -> Expr f b
    -> Expr f (t a)
    -> Expr f b

  -- | Map each element to a monoid and combine.
  foldMap ::
    P.Monoid (Expr f m) =>
    (Expr f a -> Expr f m)
    -> Expr f (t a)
    -> Expr f m
  foldMap f = foldr (\x acc -> f x P.<> acc) P.mempty

  -- | Is the mapped value empty?
  null :: Expr f (t a) -> Expr f 'Bool

  -- | Number of elements in the mapped value.
  length :: Expr f (t a) -> Expr f 'Number

  -- | Does the mapped value contain the element?
  elem :: Expr f a -> Expr f (t a) -> Expr f 'Bool

  -- | Does the mapped value not contain the element?
  notElem :: Expr f a -> Expr f (t a) -> Expr f 'Bool
  notElem x t = if_ (elem x t) (Literal (ValueBool False)) (Literal (ValueBool True))

instance Foldable Array where
  foldr f z xs = A.reduceRight xs z (\acc x -> f x acc)
  foldl f z xs = A.reduce xs z f
  null xs = A.length xs .== Literal (ValueNumber 0)
  length = A.length
  elem x = any (structuralEq x)

instance Foldable Option where
  foldr f z o = optionCase o z (\x -> f x z)
  foldl f z o = optionCase o z (\x -> f z x)
  null o = optionCase o (Literal (ValueBool True)) (\_ -> Literal (ValueBool False))
  length o = optionCase o (Literal (ValueNumber 0)) (\_ -> Literal (ValueNumber 1))
  elem x o = optionCase o (Literal (ValueBool False)) (structuralEq x)

instance Foldable (Result e) where
  foldr f z r = resultCase r (const z) (\x -> f x z)
  foldl f z r = resultCase r (const z) (\x -> f z x)
  null r =
    resultCase r (\_ -> Literal (ValueBool True)) (\_ -> Literal (ValueBool False))
  length r = resultCase r (\_ -> Literal (ValueNumber 0)) (\_ -> Literal (ValueNumber 1))
  elem x r = resultCase r (\_ -> Literal (ValueBool False)) (structuralEq x)

-- | Combine the elements of a mapped value with their 'P.Monoid'.
fold ::
  (Foldable t, P.Monoid (Expr f m)) => Expr f (t m) -> Expr f m
fold = foldMap P.id

-- | Flatten a nested monoidal value.
concat :: (Foldable t, P.Monoid (Expr f m)) => Expr f (t m) -> Expr f m
concat = fold

-- | Map each element to a monoid, then flatten.
concatMap ::
  (Foldable t, P.Monoid (Expr f m)) =>
  (Expr f a -> Expr f m)
  -> Expr f (t a)
  -> Expr f m
concatMap = foldMap

-- | Conjunction of a mapped boolean value.
and :: Foldable t => Expr f (t 'Bool) -> Expr f 'Bool
and =
  foldl
    (\a b -> if_ a b (Literal (ValueBool False)))
    (Literal (ValueBool True))

-- | Disjunction of a mapped boolean value.
or :: Foldable t => Expr f (t 'Bool) -> Expr f 'Bool
or = foldl (.||) (Literal (ValueBool False))

-- | Does any element satisfy the predicate?
any :: Foldable t => (Expr f a -> Expr f 'Bool) -> Expr f (t a) -> Expr f 'Bool
any p = foldl (\acc x -> acc .|| p x) (Literal (ValueBool False))

-- | Do all elements satisfy the predicate?
all :: Foldable t => (Expr f a -> Expr f 'Bool) -> Expr f (t a) -> Expr f 'Bool
all p =
  foldl
    (\acc x -> if_ acc (p x) (Literal (ValueBool False)))
    (Literal (ValueBool True))

-- | Universe-indexed 'Data.Traversable.Traversable'.
class (Functor t, Foldable t) => Traversable t where
  -- | Map each element to an action and collect the results.
  traverse ::
    Applicative u =>
    (Expr f a -> Expr f (u b))
    -> Expr f (t a)
    -> Expr f (u (t b))

instance Traversable Array where
  traverse g xs =
    foldl
      (\acc x -> liftA2 (\as b -> A.concat as (A.singleton b)) acc (g x))
      (pure emptyArray)
      xs

instance Traversable Option where
  traverse g o = optionCase o (pure none) (\x -> fmap some (g x))

instance Traversable (Result e) where
  traverse g r =
    resultCase r (\e' -> pure (err e')) (\x -> fmap ok (g x))

-- | 'traverse' with the identity function.
sequenceA ::
  (Traversable t, Applicative u) =>
  Expr f (t (u a))
  -> Expr f (u (t a))
sequenceA = traverse P.id

-- | 'traverse' specialized to 'Monad'.
mapM ::
  (Traversable t, Monad u) =>
  (Expr f a -> Expr f (u b))
  -> Expr f (t a)
  -> Expr f (u (t b))
mapM = traverse

-- | Evaluate each action and collect the results.
sequence ::
  (Traversable t, Monad u) => Expr f (t (u a)) -> Expr f (u (t a))
sequence = sequenceA

-- | Flipped 'traverse'.
for ::
  (Traversable t, Applicative u) =>
  Expr f (t a)
  -> (Expr f a -> Expr f (u b))
  -> Expr f (u (t b))
for = P.flip traverse

-- | Flipped 'mapM'.
forM ::
  (Traversable t, Monad u) =>
  Expr f (t a)
  -> (Expr f a -> Expr f (u b))
  -> Expr f (u (t b))
forM = for

-- | 'traverse' for effects, discarding the results.
traverse_ ::
  (Foldable t, Applicative u) =>
  (Expr f a -> Expr f (u b))
  -> Expr f (t a)
  -> Expr f (u 'Unit)
traverse_ g = foldl (\acc x -> acc *> void (g x)) (pure (Literal ValueUnit))

-- | Flipped 'traverse_'.
for_ ::
  (Foldable t, Applicative u) =>
  Expr f (t a) -> (Expr f a -> Expr f (u b)) -> Expr f (u 'Unit)
for_ = P.flip traverse_

-- | 'traverse_' specialized to 'Monad'.
mapM_ ::
  (Foldable t, Monad u) =>
  (Expr f a -> Expr f (u b)) -> Expr f (t a) -> Expr f (u 'Unit)
mapM_ = traverse_

-- | Flipped 'mapM_'.
forM_ ::
  (Foldable t, Monad u) =>
  Expr f (t a) -> (Expr f a -> Expr f (u b)) -> Expr f (u 'Unit)
forM_ = for_

-- | Evaluate each action, discarding the results.
sequence_ :: (Foldable t, Monad u) => Expr f (t (u a)) -> Expr f (u 'Unit)
sequence_ = traverse_ P.id

-- | Universe-indexed 'Data.Bifunctor.Bifunctor' (here: 'Result').
class Bifunctor (p :: Universe -> Universe -> Universe) where
  -- | Map both parameters of a two-parameter value.
  bimap ::
    (Expr f a -> Expr f b)
    -> (Expr f c -> Expr f d)
    -> Expr f (p a c)
    -> Expr f (p b d)

instance Bifunctor Result where
  bimap f g r = resultCase r (err P.. f) (ok P.. g)

-- | Map the first parameter of a two-parameter value.
first ::
  Bifunctor p => (Expr f a -> Expr f b) -> Expr f (p a c) -> Expr f (p b c)
first f = bimap f P.id

-- | Map the second parameter of a two-parameter value.
second ::
  Bifunctor p => (Expr f c -> Expr f d) -> Expr f (p a c) -> Expr f (p a d)
second = bimap P.id

-- | Universe-indexed 'Data.Bifoldable.Bifoldable'.
class Bifoldable (p :: Universe -> Universe -> Universe) where
  -- | Fold both parameters of a two-parameter value.
  bifoldMap ::
    P.Monoid (Expr f m) =>
    (Expr f a -> Expr f m)
    -> (Expr f b -> Expr f m)
    -> Expr f (p a b)
    -> Expr f m

instance Bifoldable Result where
  bifoldMap f g r = resultCase r f g

-- | Universe-indexed 'Data.Bitraversable.Bitraversable'.
class (Bifunctor p, Bifoldable p) => Bitraversable p where
  -- | Traverse both parameters of a two-parameter value.
  bitraverse ::
    Applicative u =>
    (Expr f a -> Expr f (u c))
    -> (Expr f b -> Expr f (u d))
    -> Expr f (p a b)
    -> Expr f (u (p c d))

instance Bitraversable Result where
  bitraverse f g r =
    resultCase r (\e' -> fmap err (f e')) (\x -> fmap ok (g x))

-- | Sequence both parameters of a two-parameter value.
bisequence ::
  (Bitraversable p, Applicative u) =>
  Expr f (p (u a) (u b))
  -> Expr f (u (p a b))
bisequence = bitraverse P.id P.id

-- | Universe-indexed 'Control.Category.Category' (here: 'Function').
class Category (t :: Universe -> Universe -> Universe) where
  -- | The identity object-language function.
  id :: Expr f (t a a)

  -- | Object-language function composition.
  (.) :: Expr f (t b c) -> Expr f (t a b) -> Expr f (t a c)

instance Category Function where
  id = lambda P.id
  g . h = lambda (\x -> apply g (apply h x))
