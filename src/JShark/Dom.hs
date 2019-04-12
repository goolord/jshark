{-# language DataKinds #-}
{-# language OverloadedStrings #-}
{-# language ExistentialQuantification #-}
{-# language GADTs #-}
{-# language BangPatterns #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language ScopedTypeVariables #-}

module JShark.Dom where

import JShark
import JShark.Api
import JShark.Types
import Topaz.Rec ((<:))
import Topaz.Types

lookupId :: Expr f 'String -> EffectSyntax f (Effect f 'Element)
lookupId x = pure $ ffi "document.getElementById" (x <: RecNil)

lookupSelector :: Expr f 'String -> EffectSyntax f (Effect f ('Array 'Element))
lookupSelector x = pure $ ffi "document.getElementById" (x <: RecNil)
