{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}

module JShark.Object where

import Data.Kind
import Data.Proxy
import GHC.TypeLits
import JShark
import JShark.Types

type family Field (r :: Type) (k :: Symbol) :: Universe

get :: forall k r f. KnownSymbol k => Effect f ('Object r) -> EffectSyntax f (Expr f (Field r k))
get x = fmap Var $ toSyntax $ UnsafeObject x (symbolVal (Proxy :: Proxy k))

getCall :: forall k r f u. (KnownSymbol k, Field r k ~ 'Effectful u) => Effect f ('Object r) -> EffectSyntax f (Expr f u)
getCall x = fmap Var $ toSyntax $ UnsafeObject x $ (symbolVal (Proxy :: Proxy k)) <> "()"

call :: Expr f ('Effectful u) -> EffectSyntax f (Expr f u)
call e = do
  x <- toSyntax (UnEffectful e)
  pure (Var x)

call_ :: Expr f ('Effectful 'Unit) -> EffectSyntax f ()
call_ e = do
  _ <- toSyntax $ UnEffectful e
  pure ()

