{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- | 'Generic' product records and tagged sums as JShark objects.
Records: row 'As' @a@ (or @type instance Field a k = ViaGeneric a k@).
Sums: @{tag, payload}@ on row 'Tagged' @a@.
-}
module JShark.Generic
  ( As
  , MutableObjectOf
  , Tagged
  , SumOf
  , Payload
  , CtorU
  , UniverseOf
  , FieldU
  , GField
  , ViaGeneric
  , ToJS (..)
  , ToValue (..)
  , toObject
  , toObjectArray
  , newRecord
  , toSum
  , toSumArray
  , CtorNames
  , CaseSum (..)
  , data Case_
  , on
  , caseSum
  , whenTag
  , sumTag
  )
where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import GHC.TypeLits
  ( ErrorMessage (..)
  , KnownSymbol
  , Nat
  , Symbol
  , TypeError
  , symbolVal
  , type (+)
  )
import JShark.Api
  ( bool
  , expr
  , hold
  , ifE
  , none
  , number
  , some
  , string
  , throw_
  , (.==)
  )
import JShark.Array (fromEffects)
import JShark.Object (field, get, newObject, obj, unsafeObjectGet)
import JShark.Types
import Unsafe.Coerce (unsafeCoerce)

{- | Row phantom for a 'Generic' record @a@. Existing rows ('Window')
stay manual; they do not become 'As'.
-}
data As (a :: Type)

-- | @'MutableObject' ('As' a)@
type MutableObjectOf a = 'MutableObject (As a)

type instance Field (As a) k = GField a k

-- | Use @a@ as its own row: @type instance Field a k = ViaGeneric a k@.
type ViaGeneric a k = GField a k

-- | Field universe at key @k@, from @Rep a@.
type family GField (a :: Type) (k :: Symbol) :: Universe where
  GField a k = GFieldRep (Rep a) k

type family GFieldRep (r :: Type -> Type) (k :: Symbol) :: Universe where
  GFieldRep (D1 _ f) k = GFieldRep f k
  GFieldRep (C1 _ f) k = GFieldRep f k
  GFieldRep (S1 ('MetaSel ('Just k) _ _ _) (Rec0 a)) k = FieldU a
  GFieldRep (S1 ('MetaSel ('Just _) _ _ _) _) k =
    TypeError ('Text "JShark.Generic: no field " ':<>: 'ShowType k)
  GFieldRep (S1 ('MetaSel 'Nothing _ _ _) _) _ =
    TypeError
      ('Text "JShark.Generic: positional fields not supported; use record selectors")
  GFieldRep (l :*: r) k = GFieldProd (GHasField l k) l r k
  GFieldRep (_ :+: _) _ =
    TypeError ('Text "JShark.Generic: sum types are not records")
  GFieldRep U1 k =
    TypeError ('Text "JShark.Generic: no field " ':<>: 'ShowType k)
  GFieldRep V1 _ =
    TypeError ('Text "JShark.Generic: void type")

type family GHasField (r :: Type -> Type) (k :: Symbol) :: Bool where
  GHasField (S1 ('MetaSel ('Just k) _ _ _) _) k = 'True
  GHasField (S1 _ _) _ = 'False
  GHasField (l :*: r) k = OrBool (GHasField l k) (GHasField r k)
  GHasField (D1 _ f) k = GHasField f k
  GHasField (C1 _ f) k = GHasField f k
  GHasField U1 _ = 'False

type family
  GFieldProd (has :: Bool) (l :: Type -> Type) (r :: Type -> Type) (k :: Symbol) ::
    Universe
  where
  GFieldProd 'True l _ k = GFieldRep l k
  GFieldProd 'False _ r k = GFieldRep r k

type family OrBool (a :: Bool) (b :: Bool) :: Bool where
  OrBool 'True _ = 'True
  OrBool 'False b = b

-- | Leaf universes shared by 'UniverseOf' and 'FieldU'. Not exported.
type family ScalarU (a :: Type) :: Universe where
  ScalarU Double = 'Number
  ScalarU Float = 'Number
  ScalarU Int = 'Number
  ScalarU Text = 'String
  ScalarU Bool = 'Bool
  ScalarU () = 'Unit

{- | Host type → JShark universe for 'ToJS' / 'ToValue' only. No
catch-all: records are 'toObject', not 'Expr'.
-}
type family UniverseOf (a :: Type) :: Universe where
  UniverseOf Double = ScalarU Double
  UniverseOf Float = ScalarU Float
  UniverseOf Int = ScalarU Int
  UniverseOf Text = ScalarU Text
  UniverseOf Bool = ScalarU Bool
  UniverseOf () = ScalarU ()
  UniverseOf [a] = 'Array (UniverseOf a)
  UniverseOf (Maybe a) = 'Option (UniverseOf a)
  UniverseOf (Either e a) = 'Result (UniverseOf e) (UniverseOf a)

{- | Field-position universe. Nested products use 'As'; nested sums use
'Tagged'.
-}
type family FieldU (a :: Type) :: Universe where
  FieldU Double = ScalarU Double
  FieldU Float = ScalarU Float
  FieldU Int = ScalarU Int
  FieldU Text = ScalarU Text
  FieldU Bool = ScalarU Bool
  FieldU () = ScalarU ()
  FieldU [a] = 'Array (FieldU a)
  FieldU (Maybe a) = 'Option (FieldU a)
  FieldU (Either e a) = 'Result (FieldU e) (FieldU a)
  FieldU a = 'MutableObject (RowOf a)

type family RowOf (a :: Type) :: Type where
  RowOf a = RowOfRep a (Rep a)

type family RowOfRep (a :: Type) (r :: Type -> Type) :: Type where
  RowOfRep a (D1 _ f) = RowOfRep a f
  RowOfRep a (_ :+: _) = Tagged a
  RowOfRep a _ = As a

-- | Host value as a pure 'Expr'. Primitives only; records use 'toObject'.
class ToJS a where
  toJS :: a -> Expr f (UniverseOf a)

-- | Host ↔ 'Value' for universes that 'evaluate' can inhabit.
class ToJS a => ToValue a where
  toValue :: a -> Value (UniverseOf a)
  fromValue :: Value (UniverseOf a) -> a

instance ToJS Double where
  toJS = number

instance ToValue Double where
  toValue = ValueNumber
  fromValue (ValueNumber d) = d

instance ToJS Float where
  toJS = number . realToFrac

instance ToValue Float where
  toValue = ValueNumber . realToFrac
  fromValue (ValueNumber d) = realToFrac d

{- | IEEE 'Number'. Integers outside (-2^53, 2^53) round. 'fromValue'
uses 'truncate' (toward 0), matching a host 'toJS' roundtrip, not
JS ToInt32.
-}
instance ToJS Int where
  toJS = number . fromIntegral

instance ToValue Int where
  toValue = ValueNumber . fromIntegral
  fromValue (ValueNumber d) = truncate d

instance ToJS Text where
  toJS = string

instance ToValue Text where
  toValue = ValueString
  fromValue (ValueString s) = s

instance ToJS Bool where
  toJS = bool

instance ToValue Bool where
  toValue = ValueBool
  fromValue (ValueBool b) = b

instance ToJS () where
  toJS _ = Literal ValueUnit

instance ToValue () where
  toValue _ = ValueUnit
  fromValue ValueUnit = ()

instance ToValue a => ToJS [a] where
  toJS = Literal . toValue

instance ToValue a => ToValue [a] where
  toValue = ValueArray . map toValue
  fromValue (ValueArray xs) = map fromValue xs

instance ToValue a => ToJS (Maybe a) where
  toJS = Literal . toValue

instance ToValue a => ToValue (Maybe a) where
  toValue = ValueOption . fmap toValue
  fromValue (ValueOption m) = fmap fromValue m

instance (ToValue e, ToValue a) => ToJS (Either e a) where
  toJS = Literal . toValue

instance (ToValue e, ToValue a) => ToValue (Either e a) where
  toValue (Left e) = ValueResult (Left (toValue e))
  toValue (Right a) = ValueResult (Right (toValue a))
  fromValue (ValueResult (Left e)) = Left (fromValue e)
  fromValue (ValueResult (Right a)) = Right (fromValue a)

-- | Record → object literal. Row is 'As' @a@. Not on 'ToJS' / 'evaluate'.
toObject ::
  (Generic a, GToObject (Rep a) (As a)) => a -> Effect f ('MutableObject (As a))
toObject = obj . gtoFields . from

-- | @[Person]@ / @todos@. One array of 'toObject' elements.
toObjectArray ::
  (Generic a, GToObject (Rep a) (As a)) =>
  [a] -> Effect f ('Array ('MutableObject (As a)))
toObjectArray = fromEffects . map toObject

-- | Empty object of row 'As' @a@. Constrained so @newRecord \@Int@ is rejected.
newRecord ::
  forall a f.
  (Generic a, GToObject (Rep a) (As a)) => Effect f ('MutableObject (As a))
newRecord = newObject
  where
    -- Mention 'toObject' so 'GToObject' is used; never applied.
    _recordRow = toObject :: a -> Effect f ('MutableObject (As a))

-- | Splice an object/sum 'Effect' into a 'FieldLit' 'Expr' hole.
embedObject :: Effect f u -> Expr f u
embedObject = UnsafeEffectExpr

impossible :: a
impossible = error "JShark.Generic: unreachable (TypeError instance)"

data FieldKind = Prim | Rec | Sum | List FieldKind | Opt FieldKind

type family KindOf (a :: Type) :: FieldKind where
  KindOf Double = 'Prim
  KindOf Float = 'Prim
  KindOf Int = 'Prim
  KindOf Text = 'Prim
  KindOf Bool = 'Prim
  KindOf () = 'Prim
  KindOf [a] = 'List (KindOf a)
  KindOf (Maybe a) = 'Opt (KindOf a)
  KindOf (Either _ _) = 'Prim
  KindOf a = KindOfRep (Rep a)

type family KindOfRep (r :: Type -> Type) :: FieldKind where
  KindOfRep (D1 _ f) = KindOfRep f
  KindOfRep (_ :+: _) = 'Sum
  KindOfRep _ = 'Rec

class DispatchField (k :: FieldKind) a where
  dispatchField :: a -> Expr f (FieldU a)

instance (ToValue a, FieldU a ~ UniverseOf a) => DispatchField 'Prim a where
  dispatchField = toJS

instance
  (Generic a, GToObject (Rep a) (As a), FieldU a ~ 'MutableObject (As a)) =>
  DispatchField 'Rec a
  where
  dispatchField = embedObject . toObject

instance
  (Generic a, GToSum a (Rep a), FieldU a ~ 'MutableObject (Tagged a)) =>
  DispatchField 'Sum a
  where
  dispatchField = embedObject . toSum

instance (ToValue a, FieldU [a] ~ UniverseOf [a]) => DispatchField ('List 'Prim) [a] where
  dispatchField = toJS

instance
  ( Generic a
  , GToObject (Rep a) (As a)
  , FieldU [a] ~ 'Array ('MutableObject (As a))
  ) =>
  DispatchField ('List 'Rec) [a]
  where
  dispatchField = embedObject . toObjectArray

instance
  (Generic a, GToSum a (Rep a), FieldU [a] ~ 'Array ('MutableObject (Tagged a))) =>
  DispatchField ('List 'Sum) [a]
  where
  dispatchField = embedObject . toSumArray

instance
  (ToValue a, FieldU (Maybe a) ~ UniverseOf (Maybe a)) =>
  DispatchField ('Opt 'Prim) (Maybe a)
  where
  dispatchField = toJS

instance
  ( Generic a
  , GToObject (Rep a) (As a)
  , FieldU (Maybe a) ~ 'Option ('MutableObject (As a))
  ) =>
  DispatchField ('Opt 'Rec) (Maybe a)
  where
  dispatchField Nothing = none
  dispatchField (Just x) = some (embedObject (toObject x))

instance
  ( Generic a
  , GToSum a (Rep a)
  , FieldU (Maybe a) ~ 'Option ('MutableObject (Tagged a))
  ) =>
  DispatchField ('Opt 'Sum) (Maybe a)
  where
  dispatchField Nothing = none
  dispatchField (Just x) = some (embedObject (toSum x))

class GToObject (r :: Type -> Type) (row :: Type) where
  gtoFields :: r x -> [FieldLit f row]

instance GToObject f row => GToObject (D1 c f) row where
  gtoFields (M1 x) = gtoFields x

instance GToObject f row => GToObject (C1 c f) row where
  gtoFields (M1 x) = gtoFields x

instance GToObject U1 row where
  gtoFields U1 = []

instance (GToObject l row, GToObject r row) => GToObject (l :*: r) row where
  gtoFields (l :*: r) = gtoFields l <> gtoFields r

instance
  (KnownSymbol k, DispatchField (KindOf t) t, Field row k ~ FieldU t) =>
  GToObject (S1 ('MetaSel ('Just k) su ss ds) (Rec0 t)) row
  where
  gtoFields (M1 (K1 x)) = [FieldLit @k (dispatchField @(KindOf t) x)]

instance
  TypeError
    ('Text "JShark.Generic: positional fields not supported; use record selectors") =>
  GToObject (S1 ('MetaSel 'Nothing su ss ds) t) row
  where
  gtoFields _ = impossible

instance
  TypeError ('Text "JShark.Generic: sum types are not records") =>
  GToObject (l :+: r) row
  where
  gtoFields _ = impossible

-- | Tagged sum row. Only 'tag' is a 'Field'; payload is 'caseSum' / 'whenTag'.
data Tagged (a :: Type)

type instance Field (Tagged a) "tag" = 'String

-- | @'MutableObject' ('Tagged' a)@ — @{tag, payload}@.
type SumOf a = 'MutableObject (Tagged a)

-- | N-ary constructor payload object (keys are field names or @"0"@..).
data Payload (a :: Type) (name :: Symbol)

type instance Field (Payload a n) k = GPayField (Rep a) n k

-- | Payload universe of constructor @name@ on @a@.
type family CtorU (a :: Type) (name :: Symbol) :: Universe where
  CtorU a n = GCtorU a n (Rep a)

type family GCtorU (a :: Type) (n :: Symbol) (r :: Type -> Type) :: Universe where
  GCtorU a n (D1 _ f) = GCtorU a n f
  GCtorU a n (C1 ('MetaCons n _ _) p) = GPayloadU a n p
  GCtorU a n (l :+: r) = GCtorPick a n (GHasCtor l n) l r
  GCtorU _ n _ =
    TypeError ('Text "JShark.Generic: no constructor " ':<>: 'ShowType n)

type family
  GCtorPick
    (a :: Type)
    (n :: Symbol)
    (has :: Bool)
    (l :: Type -> Type)
    (r :: Type -> Type) ::
    Universe
  where
  GCtorPick a n 'True l _ = GCtorU a n l
  GCtorPick a n 'False _ r = GCtorU a n r

type family GHasCtor (r :: Type -> Type) (n :: Symbol) :: Bool where
  GHasCtor (C1 ('MetaCons n _ _) _) n = 'True
  GHasCtor (C1 _ _) _ = 'False
  GHasCtor (l :+: r) n = OrBool (GHasCtor l n) (GHasCtor r n)
  GHasCtor (D1 _ f) n = GHasCtor f n
  GHasCtor _ _ = 'False

type family GPayloadU (a :: Type) (n :: Symbol) (p :: Type -> Type) :: Universe where
  GPayloadU _ _ U1 = 'Unit
  GPayloadU _ _ (S1 _ (Rec0 t)) = FieldU t
  GPayloadU a n (_ :*: _) = 'MutableObject (Payload a n)

type family GPayField (r :: Type -> Type) (n :: Symbol) (k :: Symbol) :: Universe where
  GPayField (D1 _ f) n k = GPayField f n k
  GPayField (C1 ('MetaCons n _ _) p) n k = GPayIn p 0 k
  GPayField (l :+: r) n k = GPayPick (GHasCtor l n) l r n k
  GPayField _ n _ =
    TypeError ('Text "JShark.Generic: no constructor " ':<>: 'ShowType n)

type family
  GPayPick
    (has :: Bool)
    (l :: Type -> Type)
    (r :: Type -> Type)
    (n :: Symbol)
    (k :: Symbol) ::
    Universe
  where
  GPayPick 'True l _ n k = GPayField l n k
  GPayPick 'False _ r n k = GPayField r n k

type family GPayIn (p :: Type -> Type) (ix :: Nat) (k :: Symbol) :: Universe where
  GPayIn (S1 ('MetaSel ('Just k) _ _ _) (Rec0 t)) _ k = FieldU t
  GPayIn (S1 ('MetaSel ('Just _) _ _ _) _) _ k =
    TypeError ('Text "JShark.Generic: no payload field " ':<>: 'ShowType k)
  GPayIn (S1 ('MetaSel 'Nothing _ _ _) (Rec0 t)) ix k =
    PayIfEq k (NatSym ix) (FieldU t)
  GPayIn (l :*: r) ix k =
    PayInPick (GPayHas l k ix) l r ix k
  GPayIn U1 _ k =
    TypeError ('Text "JShark.Generic: no payload field " ':<>: 'ShowType k)

type family PayIfEq (a :: Symbol) (b :: Symbol) (u :: Universe) :: Universe where
  PayIfEq a a u = u
  PayIfEq a _ _ =
    TypeError ('Text "JShark.Generic: no payload field " ':<>: 'ShowType a)

type family
  PayInPick
    (has :: Bool)
    (l :: Type -> Type)
    (r :: Type -> Type)
    (ix :: Nat)
    (k :: Symbol) ::
    Universe
  where
  PayInPick 'True l _ ix k = GPayIn l ix k
  PayInPick 'False l r ix k = GPayIn r (ix + FieldCount l) k

type family GPayHas (p :: Type -> Type) (k :: Symbol) (ix :: Nat) :: Bool where
  GPayHas (S1 ('MetaSel ('Just k) _ _ _) _) k _ = 'True
  GPayHas (S1 ('MetaSel ('Just _) _ _ _) _) _ _ = 'False
  GPayHas (S1 ('MetaSel 'Nothing _ _ _) _) k ix = SymEq k (NatSym ix)
  GPayHas (l :*: r) k ix =
    OrBool (GPayHas l k ix) (GPayHas r k (ix + FieldCount l))
  GPayHas U1 _ _ = 'False

type family SymEq (a :: Symbol) (b :: Symbol) :: Bool where
  SymEq a a = 'True
  SymEq _ _ = 'False

type family FieldCount (p :: Type -> Type) :: Nat where
  FieldCount U1 = 0
  FieldCount (S1 _ _) = 1
  FieldCount (l :*: r) = FieldCount l + FieldCount r
  FieldCount (D1 _ f) = FieldCount f
  FieldCount (C1 _ f) = FieldCount f

type family NatSym (n :: Nat) :: Symbol where
  NatSym 0 = "0"
  NatSym 1 = "1"
  NatSym 2 = "2"
  NatSym 3 = "3"
  NatSym 4 = "4"
  NatSym 5 = "5"
  NatSym 6 = "6"
  NatSym 7 = "7"
  NatSym _ =
    TypeError ('Text "JShark.Generic: at most 8 positional payload fields")

-- | Sum → @{tag: "Ctor", payload?}@.
toSum :: (Generic a, GToSum a (Rep a)) => a -> Effect f (SumOf a)
toSum = gtoSum . from

-- | @[Color]@. One array of 'toSum' elements.
toSumArray ::
  (Generic a, GToSum a (Rep a)) => [a] -> Effect f ('Array (SumOf a))
toSumArray = fromEffects . map toSum

sumTag :: Effect f (SumOf a) -> EffectSyntax f (Expr f 'String)
sumTag = get @"tag"

-- | Constructor names of @a@ in declaration order.
type family CtorNames a :: [Symbol] where
  CtorNames a = GCtorNames (Rep a)

type family GCtorNames (r :: Type -> Type) :: [Symbol] where
  GCtorNames (D1 _ f) = GCtorNames f
  GCtorNames (l :+: r) = AppendSym (GCtorNames l) (GCtorNames r)
  GCtorNames (C1 ('MetaCons n _ _) _) = '[n]
  GCtorNames V1 =
    TypeError ('Text "JShark.Generic: caseSum expects a non-empty sum type")
  GCtorNames _ =
    TypeError ('Text "JShark.Generic: caseSum expects a sum type")

type family AppendSym (xs :: [Symbol]) (ys :: [Symbol]) :: [Symbol] where
  AppendSym '[] ys = ys
  AppendSym (x ': xs) ys = x ': AppendSym xs ys

{- | Handler chain indexed by constructor names. @caseSum s arms@
requires @arms :: CaseSum a f v (CtorNames a)@. 'CaseAny' / 'Case_'
inhabit any leftover suffix (does not extend the name list).
-}
data CaseSum a f v (ns :: [Symbol]) where
  CaseEnd :: CaseSum a f v '[]
  CaseAny :: (Expr f 'String -> Effect f v) -> CaseSum a f v ns
  CaseCons ::
    forall (name :: Symbol) ns a f v.
    ( KnownSymbol name
    , Unpayload (IsUnit (CtorU a name)) (CtorU a name)
    ) =>
    (Expr f (CtorU a name) -> Effect f v)
    -> CaseSum a f v ns
    -> CaseSum a f v (name ': ns)

-- | Suffix wildcard. Same as 'CaseAny'.
pattern Case_ :: (Expr f 'String -> Effect f v) -> CaseSum a f v ns
pattern Case_ k = CaseAny k

-- | @on @"Ctor" handler rest@
on ::
  forall (name :: Symbol) a f v ns.
  ( KnownSymbol name
  , Unpayload (IsUnit (CtorU a name)) (CtorU a name)
  ) =>
  (Expr f (CtorU a name) -> Effect f v)
  -> CaseSum a f v ns
  -> CaseSum a f v (name ': ns)
on = CaseCons

{- | Exhaustive @if (s.tag === "C1") … else if …@. Every named arm
tests its tag. 'CaseEnd' throws on leftover tags; 'CaseAny' /
'Case_' is a suffix wildcard (remaining constructors + unknown).
Named arms must be a prefix of 'CtorNames' in declaration order.

@
caseSum shape $
  on @"Circle" (\\r -> …) $
  on @"Rect"   (\\p -> …) $
  CaseEnd
@

@
caseSum phase $
  on @"Play" (\\_ -> …) $
  Case_ (\\_ -> noOp)
@
-}
withTag ::
  Effect f (SumOf a)
  -> (Expr f 'String -> Effect f (SumOf a) -> Effect f v)
  -> Effect f v
withTag s k = fromSyntax $ do
  o <- hold s
  t <- get @"tag" o
  toSyntax (k t o)

caseSum ::
  forall a f v.
  Effect f (SumOf a)
  -> CaseSum a f v (CtorNames a)
  -> Effect f v
caseSum s arms = withTag s (\t o -> emitCase t o arms)

emitCase ::
  forall a f v ns r.
  Expr f 'String
  -> Effect f ('MutableObject r)
  -> CaseSum a f v ns
  -> Effect f v
emitCase t o (CaseCons @name hit rest) =
  ifE
    (expr (t .== string (T.pack (symbolVal (Proxy @name)))))
    (hit (unpayload @(IsUnit (CtorU a name)) @(CtorU a name) o))
    (emitCase t o rest)
emitCase t _ (CaseAny k) = k t
emitCase t _ CaseEnd =
  throw_ (string (T.pack "JShark.Generic: caseSum: unhandled ") <> t)

{- | @if (s.tag === "Ctor") hit(s.payload) else miss@. Nullary ctors
pass 'Unit'; they do not read @payload@. One-arm; use 'caseSum' for
an exhaustive match.
-}
whenTag ::
  forall (name :: Symbol) a f v.
  (KnownSymbol name, Unpayload (IsUnit (CtorU a name)) (CtorU a name)) =>
  Effect f (SumOf a)
  -> (Expr f (CtorU a name) -> Effect f v)
  -> Effect f v
  -> Effect f v
whenTag s hit miss =
  withTag s $ \t o ->
    emitCase @a t o (CaseCons @name @_ @a hit (CaseAny (\_ -> miss)))

type family IsUnit (u :: Universe) :: Bool where
  IsUnit 'Unit = 'True
  IsUnit _ = 'False

class Unpayload (b :: Bool) (u :: Universe) where
  unpayload :: Effect f ('MutableObject r) -> Expr f u

instance Unpayload 'True 'Unit where
  unpayload _ = Literal ValueUnit

instance Unpayload 'False u where
  unpayload o = embedObject (unsafeObjectGet o "payload")

class GToSum a (r :: Type -> Type) where
  gtoSum :: r x -> Effect f (SumOf a)

instance GToSum a f => GToSum a (D1 c f) where
  gtoSum (M1 x) = gtoSum x

instance (GToSum a l, GToSum a r) => GToSum a (l :+: r) where
  gtoSum (L1 l) = gtoSum @a l
  gtoSum (R1 r) = gtoSum @a r

instance KnownSymbol name => GToSum a (C1 ('MetaCons name fx rec) U1) where
  gtoSum _ = emitTagged @a (symbolVal (Proxy @name))

instance
  (KnownSymbol name, DispatchField (KindOf t) t) =>
  GToSum a (C1 ('MetaCons name fx rec) (S1 m (Rec0 t)))
  where
  gtoSum (M1 (M1 (K1 x))) =
    emitTaggedPayload @a (symbolVal (Proxy @name)) (dispatchField @(KindOf t) x)

instance
  (KnownSymbol name, GToPayloadN 0 (l :*: r) (Payload a name)) =>
  GToSum a (C1 ('MetaCons name fx rec) (l :*: r))
  where
  gtoSum (M1 p) =
    emitTaggedPayload @a
      (symbolVal (Proxy @name))
      (embedObject (obj (gPayloadFieldsN @0 @(l :*: r) @(Payload a name) p)))

{- | Internal row so @{tag, payload}@ can be one 'ObjectLit' without a
public 'Field' on 'Tagged'.
-}
data PayloadRow (u :: Universe)

type instance Field (PayloadRow u) "tag" = 'String

type instance Field (PayloadRow u) "payload" = u

emitTagged :: forall a f. String -> Effect f (SumOf a)
emitTagged name = obj [field @"tag" (string (T.pack name))]

emitTaggedPayload :: forall a f u. String -> Expr f u -> Effect f (SumOf a)
emitTaggedPayload name p =
  unsafeCoerce
    ( obj
        [ field @"tag" (string (T.pack name))
        , field @"payload" p
        ] ::
        Effect f ('MutableObject (PayloadRow u))
    )

class GToPayloadN (n :: Nat) (p :: Type -> Type) (row :: Type) where
  gPayloadFieldsN :: p x -> [FieldLit f row]

instance GToPayloadN n U1 row where
  gPayloadFieldsN _ = []

instance
  (KnownSymbol k, DispatchField (KindOf t) t, Field row k ~ FieldU t) =>
  GToPayloadN n (S1 ('MetaSel ('Just k) su ss ds) (Rec0 t)) row
  where
  gPayloadFieldsN (M1 (K1 x)) = [FieldLit @k (dispatchField @(KindOf t) x)]

instance
  ( KnownSymbol (NatSym n)
  , DispatchField (KindOf t) t
  , Field row (NatSym n) ~ FieldU t
  ) =>
  GToPayloadN n (S1 ('MetaSel 'Nothing su ss ds) (Rec0 t)) row
  where
  gPayloadFieldsN (M1 (K1 x)) = [FieldLit @(NatSym n) (dispatchField @(KindOf t) x)]

instance
  (GToPayloadN n l row, GToPayloadN (n + FieldCount l) r row) =>
  GToPayloadN n (l :*: r) row
  where
  gPayloadFieldsN (l :*: r) =
    gPayloadFieldsN @n l <> gPayloadFieldsN @(n + FieldCount l) r
