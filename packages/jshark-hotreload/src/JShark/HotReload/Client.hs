{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Browser hot-reload runtime bytes plus JShark EDSL lifecycle hooks
-- (@onDispose@, @hotState@) used by apps that support HMR.
module JShark.HotReload.Client
  ( clientRuntimeScript
  , clientRuntimeText
  , onDispose
  , hotState
  , hotStateGet
  , hotStateSet
  )
where

import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import JShark.Api
import JShark.Api.Rec (Rec (..), (<:))
import JShark.Api.Types
import JShark.Object (unsafeObjectAssign, unsafeObjectGet)
import Language.Haskell.TH (stringE)
import Language.Haskell.TH.Syntax
  ( makeRelativeToProject
  , qAddDependentFile
  , runIO
  )

-- | Embedded @assets/jshark-reload.js@ served at
-- @/__jshark/client.js@.
clientRuntimeScript :: ByteString
clientRuntimeScript = TE.encodeUtf8 (T.pack clientRuntimeSource)

clientRuntimeText :: Text
clientRuntimeText = TE.decodeUtf8 clientRuntimeScript

-- | Register cleanup run before a hot JS replace
-- (@window.__JSHARK_DISPOSE__@).
onDispose :: EffectSyntax f (f 'Unit) -> EffectSyntax f ()
onDispose body =
  toSyntax_ $
    unsafeObjectAssign
      (unsafeObjectGet window "__JSHARK_DISPOSE__")
      (LambdaE (\_ -> stmts body))

-- | Read previously saved hot state for @key@ (or @undefined@).
hotStateGet :: Text -> EffectSyntax f (Expr f u)
hotStateGet key =
  bindExpr $
    ffiExpr
      ( "(window.__JSHARK_HOT_STATE__&&window.__JSHARK_HOT_STATE__["
          ++ show (T.unpack key)
          ++ "])"
      )
      RecNil

-- | Write @value@ into @window.__JSHARK_HOT_STATE__[key]@.
hotStateSet :: Text -> Expr f u -> EffectSyntax f (f 'Unit)
hotStateSet key value = do
  toSyntax_ $
    ffi
      ( "(function(k,v){window.__JSHARK_HOT_STATE__=window.__JSHARK_HOT_STATE__||{};"
          ++ "window.__JSHARK_HOT_STATE__[k]=v;})"
      )
      (arg (string key) <: arg value <: RecNil)
  done

-- | Prefer restored @window.__JSHARK_HOT_STATE__[key]@; otherwise run
-- @mkInitial@ and store it. Also installs @__JSHARK_GET_STATE__@ so the
-- browser client can snapshot before HMR apply.
hotState ::
  Text
  -> EffectSyntax f (Expr f u)
  -> EffectSyntax f (Expr f u)
hotState key mkInitial = do
  state <-
    bindExpr $
      ffi
        ( "(function(k,mk){var S=window.__JSHARK_HOT_STATE__="
            ++ "window.__JSHARK_HOT_STATE__||{};"
            ++ "if(S[k]==null)S[k]=mk();return S[k];})"
        )
        ( arg (string key)
            <: ArgEffect
              ( LambdaE $ \_ ->
                  fromSyntax $ do
                    v <- mkInitial
                    yield v
              )
            <: RecNil
        )
  toSyntax_ $
    unsafeObjectAssign
      (unsafeObjectGet window "__JSHARK_GET_STATE__")
      ( LambdaE $ \_ ->
          fromSyntax $ do
            void (hotStateSet key state)
            snap <- bindExpr $ ffiExpr "(window.__JSHARK_HOT_STATE__||{})" RecNil
            yield snap
      )
  pure state

-- Rebuild this module when the browser runtime changes.
clientRuntimeSource :: String
clientRuntimeSource =
  $( do
       rel <- makeRelativeToProject "assets/jshark-reload.js"
       qAddDependentFile rel
       runIO (readFile rel) >>= stringE
   )
