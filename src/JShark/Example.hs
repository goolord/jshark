{-# language DataKinds #-}
{-# language OverloadedStrings #-}
{-# language TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module JShark.Example where

import qualified JShark.Math as Math
import JShark
import JShark.Api
import JShark.Types
import JShark.Dom

mainJS :: EffectSyntax f (f 'Unit)
mainJS = do
  nav <- lookupId "nav"
  collapse <- lookupId "collapse"
  onClick collapse $ \_ -> fromSyntax $ do
    classToggle nav "is-collapsed"
  toSyntax noOp

