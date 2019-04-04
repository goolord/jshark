{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language DeriveFunctor #-}
{-# language DerivingStrategies #-}
{-# language EmptyCase #-}
{-# language ExistentialQuantification #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeInType #-}
{-# language TypeFamilies #-}
{-# language TypeFamilyDependencies #-}
{-# language TypeOperators #-}

-- {-# options_ghc -Wall -fno-warn-unused-top-binds #-}
{-# options_ghc -w #-}

module Javascript 
  (
  ) where

{-
import Unsafe.Coerce (unsafeCoerce)
import qualified GHC.Num as Num
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)
import Key

data Universe
  = Null -- ^ null
  | Number -- ^ IEEE 754 floating point double precision
  | String -- ^ javascript strings
  | Array Universe -- ^ javascript arrays
  | JSBool -- ^ javascript bool
  | Tuple2 Universe Universe -- ^ 2-tuple. does not exist in source js.
  | Tuple3 Universe Universe Universe -- ^ 3-tuple. does not exist in source js.
  | Option Universe -- ^ option type. does not exist in source js.
  | Result Universe Universe -- ^ result type. does not exist in source js.
  | Function Universe Universe
  deriving stock (Eq)

data Value :: Universe -> Type where
  ValueNull :: Void -> Value 'Null -- ^ absurd
  ValueVar :: Int -> Value 'Null -- ^ const variables
  ValueNumber :: Double -> Value 'Number
  ValueString :: Text -> Value 'String
  ValueArray :: [Value u] -> Value ('Array u)
  ValueBool :: Bool -> Value 'JSBool
  ValueTuple2 :: Value u -> Value u' -> Value ('Tuple2 u u')
  ValueTuple3 :: Value u -> Value u' -> Value u'' -> Value ('Tuple3 u u' u'')
  ValueOption :: Maybe (Value u) -> Value ('Option u)
  ValueResult :: Value u -> Value u' -> Value ('Result u u')
  ValueFunction :: (Value u -> Value u') -> Value ('Function u u')

data Exists :: (k -> Type) -> Type where
  Exists :: !(f k) -> Exists f

data Some :: (k -> Type) -> Type where
  Some :: !(Sing a) -> !(f a) -> Some f

type family Sing = (r :: k -> Type) | r -> k

type instance Sing = SUniverse

data SUniverse :: Universe -> Type where
  SNull ::
    SUniverse 'Null
  SNumber ::
    SUniverse 'Number
  SString ::
    SUniverse 'String
  SArray ::
       SUniverse u
    -> SUniverse ('Array u)
  STuple2 ::
       SUniverse u
    -> SUniverse u'
    -> SUniverse ('Tuple2 u u')
  STuple3 ::
       SUniverse u
    -> SUniverse u'
    -> SUniverse u''
    -> SUniverse ('Tuple3 u u' u'')
  SOption ::
       SUniverse u
    -> SUniverse ('Option u)
  SResult :: 
       SUniverse u
    -> SUniverse u'
    -> SUniverse ('Result u u')
  SFunction ::
       SUniverse u
    -> SUniverse u'
    -> SUniverse ('Function u u')

deriving stock instance Eq (SUniverse u)

instance Num.Num (Value 'Number) where
  (ValueNumber a) + (ValueNumber b) = ValueNumber (a + b)
  (ValueNumber a) * (ValueNumber b) = ValueNumber (a * b)
  (ValueNumber a) - (ValueNumber b) = ValueNumber (a - b)
  abs (ValueNumber a) = ValueNumber (Num.abs a)
  signum (ValueNumber a) = ValueNumber (Num.signum a)
  fromInteger n = ValueNumber (Num.fromInteger n)

instance Semiring (Value 'Number) where
  plus = (Num.+)
  zero = ValueNumber 0
  times = (Num.*)
  one = ValueNumber 1

newtype Binding :: (Universe -> Type) -> Universe -> Type where
  Binding :: f u -> Binding f u

-- | Code Evaluation.
newtype Evaluate :: Universe -> Type where
  Evaluate :: Value u -> Evaluate u

-- | Code Generation.
newtype Generate :: Universe -> Type where
  Generate :: Int -> Generate u

-- | The type of expressions, which are pure computations.
data Expr (f :: Universe -> Type) n where
  ExprVar ::
      Int -- ^ which value we were bound to
   -> Sing u -- ^ we need this to convince GHC that the u in the callback unifies with the type of the var we are investigating. this is a bad hack. i do not like this. i do not want this. -- note: investigate using SingI
   -> (Binding f u -> n)
   -> Expr f n 
  ExprLiteral :: -- ^ Const literals.
      Value u
   -> (Binding f u -> n)
   -> Expr f n
  ExprPlus :: -- ^ IEEE 754 double-precision addition.
      Binding f 'Number
   -> Binding f 'Number
   -> (Binding f 'Number -> n)
   -> Expr f n
  ExprTimes :: -- ^ IEEE 754 double-precision multiplication.
      Binding f 'Number
   -> Binding f 'Number
   -> (Binding f 'Number -> n)
   -> Expr f n
  ExprMinus :: -- ^ IEEE 754 double-precision subtraction.
      Binding f 'Number
   -> Binding f 'Number
   -> (Binding f 'Number -> n)
   -> Expr f n
  ExprMap ::
      (Binding f u -> Binding f u')
   -> Binding f ('Array u)
   -> (Binding f ('Array u') -> n)
   -> Expr f n
  ExprFunction ::
      Binding f ('Function u u')
   -> (Binding f ('Function u u') -> n)
   -> Expr f n
  ExprAp ::
      (Binding f ('Function u u'))
   -> Binding f u
   -> (Binding f u' -> n)
   -> Expr f n

type JSE f = Free (Expr f)

-- Create a binding to an literal
literal :: Value u -> JSE f (Binding f u)
literal = \case
  v@ValueNumber{} -> liftF (ExprLiteral v id)
  v@ValueString{} -> liftF (ExprLiteral v id)
  v@ValueNull{} -> liftF (ExprLiteral v id)
  v@ValueArray{} -> liftF (ExprLiteral v id)
  v@ValueTuple2{} -> liftF (ExprLiteral v id)
  v@ValueTuple3{} -> liftF (ExprLiteral v id)
  v@ValueOption{} -> liftF (ExprLiteral v id)
  v@ValueResult{} -> liftF (ExprLiteral v id)
  v@ValueFunction{} -> liftF (ExprLiteral v id)
  v@ValueVar{} -> liftF (ExprLiteral v id)

-- | The type of statements, which are not necessarily pure computations.
data Statement (f :: Universe -> Type) n where
  Bind ::
      Value u
   -> (Binding f u -> n)
   -> Statement f n
  Log :: 
       Binding f u
    -> n
    -> Statement f n
  Foreach ::
       Binding f 'Number
    -> Binding f ('Array u)
    -> Binding f u
    -> (Binding f u  -> Free (Statement f) ())
    -> n
    -> Statement f n
  -- ^ Foreach is an inherently imperative construct. Consequently, it does not
  --   return anything.

  -- | forall (rs :: [Universe]) (res :: Universe). Declare (Rec (Binding s t) rs -> Free (Statement s t) (Binding s t res)) (Function rs res -> n)
    -- ^ Not totally sure if Declare should have the function arrow in its first arg.
  -- | forall (rs :: [Universe]) (res :: Universe). Call (Function rs res) (Rec Value rs) (Value res -> n)

deriving stock instance Functor (Statement f)
deriving stock instance Functor (Expr f)

type JSM f = Free (Statement f)

data Action f n where
  EAction :: Expr f n -> Action f n
  SAction :: Statement f n -> Action f n

deriving stock instance Functor (Action f)

type JSA f = Free (Action f)

-- | Create a binding to a not-necessarily pure variable
bind :: Value u -> JSM f (Binding f u)
bind = \case
  v@ValueNumber{} -> liftF (Bind v id)
  v@ValueString{} -> liftF (Bind v id)
  v@ValueNull{} -> liftF (Bind v id)
  v@ValueArray{} -> liftF (Bind v id)
  v@ValueBool{} -> liftF (Bind v id)
  v@ValueTuple2{} -> liftF (Bind v id)
  v@ValueTuple3{} -> liftF (Bind v id)
  v@ValueOption{} -> liftF (Bind v id)
  v@ValueResult{} -> liftF (Bind v id)
  v@ValueFunction{} -> liftF (Bind v id)
  v@ValueVar{} -> liftF (Bind v id)

interpretExpr ::
     Sing u
  -> Map Int (Some Value) -- ^ top-level bindings
  -> JSE Evaluate (Binding Evaluate u)
  -> Value u
interpretExpr u0 m = \case
  Pure (Binding (Evaluate u)) -> u
  Free b -> case b of
    ExprLiteral a g -> interpretExpr u0 m (g (Binding (Evaluate a)))
    ExprPlus x y g -> interpretExpr u0 m (g (plus_ x y))
    ExprTimes x y g -> interpretExpr u0 m (g (times_ x y))
    ExprMinus x y g -> interpretExpr u0 m (g (minus_ x y))
    ExprMap f arr g -> interpretExpr u0 m (g (map_ f arr))
    ExprFunction fuu g -> undefined --interpretExpr u0 g (fuu)
--  ExprFunction ::
--      Binding f ('Function u u')
--   -> (Binding f ('Function u u') -> n)
--   -> Expr f n
    ExprAp (Binding (Evaluate (ValueFunction f))) (Binding (Evaluate u)) g -> interpretExpr u0 m (g (Binding (Evaluate (f u))))
    ExprVar i u1 g -> case Map.lookup i m of
      Nothing -> error "scope error: make this error better"
      Just (Some u2 v) -> case decideEquality u0 u1 of
        Proved Refl -> case decideEquality u1 u2 of
          Proved Refl -> interpretExpr u2 m (g (Binding (Evaluate v)))
          Disproved _ -> error "type mismatch: make this error better"
        Disproved _ -> error "type mismatch: make this error better"

instance SDecide Universe where
  decideEquality x y
    | x == (unsafeCoerce y) = Proved (unsafeCoerce Refl)
    | otherwise = Disproved
        (\_ -> error "IMPOSSIBLE. REPORT BUG. UNSAFECOERCE IN GHC TYPE SYSTEM!!!!")

class SDecide k where
  decideEquality :: forall (a :: k) (b :: k).
       Sing a
    -> Sing b
    -> Decision (a :~: b)

data Decision :: Type -> Type where
  Proved :: a -> Decision a
  Disproved :: (a -> Void) -> Decision a

--function :: (Key s a -> Binding Evaluate u) -> Binding Evaluate u
--function = \

--function =
--var :: Identifier -> Binding Evaluate u

plus_ :: Binding Evaluate 'Number -> Binding Evaluate 'Number -> Binding Evaluate 'Number
plus_ (Binding (Evaluate (ValueNumber x))) (Binding (Evaluate (ValueNumber y)))
  = Binding (Evaluate (ValueNumber (x + y)))

times_ :: Binding Evaluate 'Number -> Binding Evaluate 'Number -> Binding Evaluate 'Number
times_ (Binding (Evaluate (ValueNumber x))) (Binding (Evaluate (ValueNumber y)))
  = Binding (Evaluate (ValueNumber (x * y)))

minus_ :: Binding Evaluate 'Number -> Binding Evaluate 'Number -> Binding Evaluate 'Number
minus_ (Binding (Evaluate (ValueNumber x))) (Binding (Evaluate (ValueNumber y)))
  = Binding (Evaluate (ValueNumber (x - y)))

map_ ::
     (Binding Evaluate u -> Binding Evaluate u')
  -> Binding Evaluate ('Array u)
  -> Binding Evaluate ('Array u')
map_ f (Binding (Evaluate (ValueArray (xs :: [Value u])))) =
  let xs' = List.map (coerce . f . coerce) xs
  in Binding (Evaluate (ValueArray xs'))

{-
equals_ :: Binding Evaluate u -> Binding Evaluate u -> Binding Evaluate 'JSBool
equals_ (Binding (Evaluate x)) (Binding (Evaluate y)) = Binding (Evaluate (ValueBool (x == y)))

ifThenElse_ :: Binding Evaluate 'JSBool -> Binding Evaluate u -> Binding Evaluate u -> Binding Evaluate u
ifThenElse_ (Binding (Evaluate (ValueBool bool'))) a b =
  if bool'
  then a
  else b
-}

identity :: Value ('Function Number Number)
identity = interpretExpr (SFunction SNumber SNumber) mempty $ do
  x <- literal $ ValueFunction id
  pure x

--identityOf :: Value 'Number
--identityOf 
--ValueFunction :: (Value u -> Value u') -> Value ('Function u u')

{-
exampleMap :: Value ('Array 'Number)
exampleMap = interpretExpr $ do
  x <- literal $ ValueArray [1,3,5]
  let fx = map_ (plus_ (Binding (Evaluate 1))) x
  pure fx      

example :: Value 'Number
example = interpretExpr $ do
  x <- literal 2
  y <- literal 3
  xy <- plus' x y
  pure xy

plus' :: Binding f 'Number -> Binding f 'Number -> JSE f (Binding f 'Number)
plus' x y = liftF $ Plus x y id

-}

-}
