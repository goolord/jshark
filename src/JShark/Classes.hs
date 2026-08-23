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
module JShark.Classes
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
import qualified JShark.Array as A
import JShark.Types
import Prelude
  ( Bool (False, True)
  , const
  , mempty
  , ($)
  , (<>)
  )
import qualified Prelude as P

class Functor (t :: Universe -> Universe) where
  fmap :: (Expr f a -> Expr f b) -> Expr f (t a) -> Expr f (t b)
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

void :: Functor t => Expr f (t a) -> Expr f (t 'Unit)
void = (Literal ValueUnit <$)

class Functor t => Applicative t where
  pure :: Expr f a -> Expr f (t a)
  liftA2 ::
    (Expr f a -> Expr f b -> Expr f c)
    -> Expr f (t a)
    -> Expr f (t b)
    -> Expr f (t c)
  (<*>) :: Expr f (t (Function a b)) -> Expr f (t a) -> Expr f (t b)
  (<*>) = liftA2 apply
  (*>) :: Expr f (t a) -> Expr f (t b) -> Expr f (t b)
  (*>) = liftA2 (const P.id)
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

liftA :: Applicative t => (Expr f a -> Expr f b) -> Expr f (t a) -> Expr f (t b)
liftA f = liftA2 (const f) (pure (Literal ValueUnit))

liftA3 ::
  Applicative t =>
  (Expr f a -> Expr f b -> Expr f c -> Expr f d)
  -> Expr f (t a)
  -> Expr f (t b)
  -> Expr f (t c)
  -> Expr f (t d)
liftA3 f x y z = liftA2 (\a b -> lambda (f a b)) x y <*> z

class Applicative t => Alternative t where
  empty :: Expr f (t a)
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

guard :: Alternative t => Expr f 'Bool -> Expr f (t 'Unit)
guard c = if_ c (pure (Literal ValueUnit)) empty

class Applicative t => Monad t where
  (>>=) :: Expr f (t a) -> (Expr f a -> Expr f (t b)) -> Expr f (t b)

instance Monad Array where
  xs >>= k = A.reduce xs emptyArray (\acc x -> A.concat acc (k x))

instance Monad Option where
  o >>= k = optionCase o none k

instance Monad (Result e) where
  r >>= k = resultCase r err k

instance Monad (Function r) where
  g >>= k = lambda (\r -> apply (k (apply g r)) r)

return :: Monad t => Expr f a -> Expr f (t a)
return = pure

(>>) :: Monad t => Expr f (t a) -> Expr f (t b) -> Expr f (t b)
(>>) = (*>)

(=<<) :: Monad t => (Expr f a -> Expr f (t b)) -> Expr f (t a) -> Expr f (t b)
(=<<) = P.flip (>>=)

(>=>) ::
  Monad t =>
  (Expr f a -> Expr f (t b))
  -> (Expr f b -> Expr f (t c))
  -> Expr f a
  -> Expr f (t c)
f >=> g = \x -> f x >>= g

(<=<) ::
  Monad t =>
  (Expr f b -> Expr f (t c))
  -> (Expr f a -> Expr f (t b))
  -> Expr f a
  -> Expr f (t c)
(<=<) = P.flip (>=>)

ap :: Monad t => Expr f (t (Function a b)) -> Expr f (t a) -> Expr f (t b)
ap = (<*>)

join :: Monad t => Expr f (t (t a)) -> Expr f (t a)
join m = m >>= P.id

when :: Monad t => Expr f 'Bool -> Expr f (t 'Unit) -> Expr f (t 'Unit)
when c m = if_ c m (pure (Literal ValueUnit))

unless :: Monad t => Expr f 'Bool -> Expr f (t 'Unit) -> Expr f (t 'Unit)
unless c = when (not_ c)

class (Alternative t, Monad t) => MonadPlus t where
  mzero :: Expr f (t a)
  mzero = empty
  mplus :: Expr f (t a) -> Expr f (t a) -> Expr f (t a)
  mplus = (<|>)

instance MonadPlus Array

instance MonadPlus Option

class Monad t => MonadFail t where
  fail :: Expr f 'String -> Expr f (t a)

instance MonadFail Array where
  fail _ = empty

instance MonadFail Option where
  fail _ = empty

mfilter ::
  MonadPlus t => (Expr f a -> Expr f 'Bool) -> Expr f (t a) -> Expr f (t a)
mfilter p m = m >>= \x -> if_ (p x) (pure x) mzero

class Monad t => MonadZip t where
  mzipWith ::
    (Expr f a -> Expr f b -> Expr f c)
    -> Expr f (t a)
    -> Expr f (t b)
    -> Expr f (t c)

instance MonadZip Array where
  mzipWith = A.zipWith

class Monad t => MonadFix t where
  mfix :: (Expr f a -> Expr f (t a)) -> Expr f (t a)

instance MonadFix (Function r) where
  mfix k = lambda (\r -> letRec (\a -> apply (k a) r) P.id)

class Foldable (t :: Universe -> Universe) where
  foldr ::
    (Expr f a -> Expr f b -> Expr f b)
    -> Expr f b
    -> Expr f (t a)
    -> Expr f b
  foldl ::
    (Expr f b -> Expr f a -> Expr f b)
    -> Expr f b
    -> Expr f (t a)
    -> Expr f b
  foldMap ::
    P.Monoid (Expr f m) =>
    (Expr f a -> Expr f m)
    -> Expr f (t a)
    -> Expr f m
  foldMap f = foldr (\x acc -> f x P.<> acc) P.mempty
  null :: Expr f (t a) -> Expr f 'Bool
  length :: Expr f (t a) -> Expr f 'Number
  elem :: Expr f a -> Expr f (t a) -> Expr f 'Bool
  notElem :: Expr f a -> Expr f (t a) -> Expr f 'Bool
  notElem x t = if_ (elem x t) (Literal (ValueBool False)) (Literal (ValueBool True))

instance Foldable Array where
  foldr f z xs = A.reduceRight xs z (\acc x -> f x acc)
  foldl f z xs = A.reduce xs z f
  null xs = A.length xs .== Literal (ValueNumber 0)
  length = A.length
  elem x = any (x .==)

instance Foldable Option where
  foldr f z o = optionCase o z (\x -> f x z)
  foldl f z o = optionCase o z (\x -> f z x)
  null o = optionCase o (Literal (ValueBool True)) (\_ -> Literal (ValueBool False))
  length o = optionCase o (Literal (ValueNumber 0)) (\_ -> Literal (ValueNumber 1))
  elem x o = optionCase o (Literal (ValueBool False)) (x .==)

instance Foldable (Result e) where
  foldr f z r = resultCase r (const z) (\x -> f x z)
  foldl f z r = resultCase r (const z) (\x -> f z x)
  null r =
    resultCase r (\_ -> Literal (ValueBool True)) (\_ -> Literal (ValueBool False))
  length r = resultCase r (\_ -> Literal (ValueNumber 0)) (\_ -> Literal (ValueNumber 1))
  elem x r = resultCase r (\_ -> Literal (ValueBool False)) (x .==)

fold ::
  (Foldable t, P.Monoid (Expr f m)) => Expr f (t m) -> Expr f m
fold = foldMap P.id

concat :: (Foldable t, P.Monoid (Expr f m)) => Expr f (t m) -> Expr f m
concat = fold

concatMap ::
  (Foldable t, P.Monoid (Expr f m)) =>
  (Expr f a -> Expr f m)
  -> Expr f (t a)
  -> Expr f m
concatMap = foldMap

and :: Foldable t => Expr f (t 'Bool) -> Expr f 'Bool
and =
  foldl
    (\a b -> if_ a b (Literal (ValueBool False)))
    (Literal (ValueBool True))

or :: Foldable t => Expr f (t 'Bool) -> Expr f 'Bool
or = foldl (.||) (Literal (ValueBool False))

any :: Foldable t => (Expr f a -> Expr f 'Bool) -> Expr f (t a) -> Expr f 'Bool
any p = foldl (\acc x -> acc .|| p x) (Literal (ValueBool False))

all :: Foldable t => (Expr f a -> Expr f 'Bool) -> Expr f (t a) -> Expr f 'Bool
all p =
  foldl
    (\acc x -> if_ acc (p x) (Literal (ValueBool False)))
    (Literal (ValueBool True))

class (Functor t, Foldable t) => Traversable t where
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

sequenceA ::
  (Traversable t, Applicative u) =>
  Expr f (t (u a))
  -> Expr f (u (t a))
sequenceA = traverse P.id

mapM ::
  (Traversable t, Monad u) =>
  (Expr f a -> Expr f (u b))
  -> Expr f (t a)
  -> Expr f (u (t b))
mapM = traverse

sequence ::
  (Traversable t, Monad u) => Expr f (t (u a)) -> Expr f (u (t a))
sequence = sequenceA

for ::
  (Traversable t, Applicative u) =>
  Expr f (t a)
  -> (Expr f a -> Expr f (u b))
  -> Expr f (u (t b))
for = P.flip traverse

forM ::
  (Traversable t, Monad u) =>
  Expr f (t a)
  -> (Expr f a -> Expr f (u b))
  -> Expr f (u (t b))
forM = for

traverse_ ::
  (Foldable t, Applicative u) =>
  (Expr f a -> Expr f (u b))
  -> Expr f (t a)
  -> Expr f (u 'Unit)
traverse_ g = foldl (\acc x -> acc *> void (g x)) (pure (Literal ValueUnit))

for_ ::
  (Foldable t, Applicative u) =>
  Expr f (t a) -> (Expr f a -> Expr f (u b)) -> Expr f (u 'Unit)
for_ = P.flip traverse_

mapM_ ::
  (Foldable t, Monad u) =>
  (Expr f a -> Expr f (u b)) -> Expr f (t a) -> Expr f (u 'Unit)
mapM_ = traverse_

forM_ ::
  (Foldable t, Monad u) =>
  Expr f (t a) -> (Expr f a -> Expr f (u b)) -> Expr f (u 'Unit)
forM_ = for_

sequence_ :: (Foldable t, Monad u) => Expr f (t (u a)) -> Expr f (u 'Unit)
sequence_ = traverse_ P.id

class Bifunctor (p :: Universe -> Universe -> Universe) where
  bimap ::
    (Expr f a -> Expr f b)
    -> (Expr f c -> Expr f d)
    -> Expr f (p a c)
    -> Expr f (p b d)

instance Bifunctor Result where
  bimap f g r = resultCase r (err P.. f) (ok P.. g)

first ::
  Bifunctor p => (Expr f a -> Expr f b) -> Expr f (p a c) -> Expr f (p b c)
first f = bimap f P.id

second ::
  Bifunctor p => (Expr f c -> Expr f d) -> Expr f (p a c) -> Expr f (p a d)
second = bimap P.id

class Bifoldable (p :: Universe -> Universe -> Universe) where
  bifoldMap ::
    P.Monoid (Expr f m) =>
    (Expr f a -> Expr f m)
    -> (Expr f b -> Expr f m)
    -> Expr f (p a b)
    -> Expr f m

instance Bifoldable Result where
  bifoldMap f g r = resultCase r f g

class (Bifunctor p, Bifoldable p) => Bitraversable p where
  bitraverse ::
    Applicative u =>
    (Expr f a -> Expr f (u c))
    -> (Expr f b -> Expr f (u d))
    -> Expr f (p a b)
    -> Expr f (u (p c d))

instance Bitraversable Result where
  bitraverse f g r =
    resultCase r (\e' -> fmap err (f e')) (\x -> fmap ok (g x))

bisequence ::
  (Bitraversable p, Applicative u) =>
  Expr f (p (u a) (u b))
  -> Expr f (u (p a b))
bisequence = bitraverse P.id P.id

class Category (t :: Universe -> Universe -> Universe) where
  id :: Expr f (t a a)
  (.) :: Expr f (t b c) -> Expr f (t a b) -> Expr f (t a c)

instance Category Function where
  id = lambda P.id
  g . h = lambda (\x -> apply g (apply h x))
