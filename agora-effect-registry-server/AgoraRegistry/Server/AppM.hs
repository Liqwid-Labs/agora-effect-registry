{-# LANGUAGE TemplateHaskell #-}

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
  registryInfo,
  encodeDatum,
) where

import Data.Aeson qualified as Aeson
import Data.Map qualified as Map
import Development.GitRev (gitBranch, gitHash)

import AgoraRegistry.DatumValidation (validateEffectDatum)
import AgoraRegistry.Schema (EffectSchema)
import AgoraRegistry.Server.EffectRegistry (EffectRegistry (EffectRegistry))
import AgoraRegistry.Server.Options (RegistryInfo (RegistryInfo), SchemaInfo (SchemaInfo), hostedEffectDatumSchemas, revision)
import AgoraRegistry.Server.Types (EffectDatum (EffectDatum), EffectScriptHash (EffectScriptHash))
import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Text (Text)
import Data.Traversable (for)
import Optics.Core ((%))
import Optics.Getter (view)
import Servant (Handler, ServerError (errBody), err400, err404, throwError)
import System.Directory.Extra (listDirectory)

-- | A wrapper over a server request handler providing state.
newtype AppM a = AppM (ReaderT EffectRegistry Handler a)
  deriving newtype (Functor, Applicative, Monad, MonadReader EffectRegistry, MonadIO)

-- | Get the server request handler.
runApp :: EffectRegistry -> AppM a -> Handler a
runApp r (AppM fh) = runReaderT fh r

-- | Get an effect schema by effect script hash.
askEffect :: EffectScriptHash -> AppM EffectSchema
askEffect hash = do
  r <- ask
  case lookupEffectByScriptHash hash r of
    Nothing ->
      AppM $
        throwError $
          err404 {errBody = "Effect with given script is not registered."}
    Just eff -> pure eff

-- | Encode datum using schema for effect with given script hash.
encodeDatum :: EffectScriptHash -> Aeson.Value -> AppM EffectDatum
encodeDatum hash jsonDatum = do
  r <- ask
  case lookupEffectByScriptHash hash r of
    Nothing ->
      AppM $
        throwError $
          err404 {errBody = "Effect with given script is not registered."}
    Just _ -> do
      effSchema <- askEffect hash
      let dataE = validateEffectDatum effSchema jsonDatum
      case dataE of
        Left x ->
          AppM $
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

registryInfo :: FilePath -> AppM RegistryInfo
registryInfo effectsPath = do
  effectFiles <- liftIO $ listDirectory effectsPath

  -- This encodes the git revision of the server. It's useful for the caller
  -- to be able to ensure they are compatible with it.
  let revision :: Text
      revision = $(gitBranch) <> "@" <> $(gitHash)

      err = AppM $ throwError $ err400 {errBody = "Could not decode effect file"}

  effectSchemas <- for effectFiles $ maybe err pure <=< liftIO . Aeson.decodeFileStrict @EffectSchema . ((effectsPath <> "/") <>)

  pure $
    RegistryInfo
      { revision = revision
      , hostedEffectDatumSchemas =
          [ SchemaInfo
            (view (#metadata % #name) effectSchema)
            (EffectScriptHash $ view #scriptHash effectSchema)
          | effectSchema <- effectSchemas
          ]
      }
