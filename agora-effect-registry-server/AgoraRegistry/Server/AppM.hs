{- |
Module     : AgoraRegistry.Server.AppM
Maintainer : michal@mlabs.city
Description: Provides API implementation.

Provides API implementation.
-}
module AgoraRegistry.Server.AppM (
  AppM (..),
  lookupEffectByScriptHash,
  runApp,
  askEffect,
  encodeDatum,
) where

import qualified Data.Aeson as Aeson
import qualified Data.Map as Map

import AgoraRegistry.DatumValidation (validateEffectDatum)
import AgoraRegistry.Schema (EffectSchema)
import AgoraRegistry.Server.Api (EffectDatum (EffectDatum), EffectScriptHash (EffectScriptHash))
import AgoraRegistry.Server.EffectRegistry (EffectRegistry (EffectRegistry))
import Data.ByteString.Lazy.UTF8 (fromString)
import Servant (Handler, ServerError (errBody), err400, err404, throwError)

-- | A wrapper over a server request handler providing state.
newtype AppM a = AppM (EffectRegistry -> Handler a)

-- | Get the server request handler.
runApp :: EffectRegistry -> AppM a -> Handler a
runApp r (AppM fh) = fh r

-- | Get an effect schema by effect script hash.
askEffect :: EffectScriptHash -> AppM EffectSchema
askEffect hash = AppM $ \r -> case lookupEffectByScriptHash hash r of
  Nothing ->
    throwError $
      err404 {errBody = "Effect with given script is not registered."}
  Just eff -> pure eff

-- | Encode datum using schema for effect with given script hash.
encodeDatum :: EffectScriptHash -> Aeson.Value -> AppM EffectDatum
encodeDatum hash jsonDatum = AppM $
  \r -> case lookupEffectByScriptHash hash r of
    Nothing ->
      throwError $
        err404 {errBody = "Effect with given script is not registered."}
    Just _ -> do
      effSchema <- runApp r $ askEffect hash
      let dataE = validateEffectDatum effSchema jsonDatum
      case dataE of
        Left x ->
          throwError $
            err400
              { errBody =
                  "Provided datum failed to encode against the effect schema. "
                    <> fromString x
              }
        Right d -> pure $ EffectDatum d

lookupEffectByScriptHash ::
  EffectScriptHash ->
  EffectRegistry ->
  Maybe EffectSchema
lookupEffectByScriptHash (EffectScriptHash k) (EffectRegistry m) = Map.lookup k m
