{- |
Module     : AgoraRegistry.Server.EffectRegistry
Maintainer : michal@mlabs.city
Description: Defines EffectRegistry types and functions.

Defines EffectRegistry types and functions.
-}
module AgoraRegistry.Server.EffectRegistry (
  EffectRegistry (EffectRegistry),
  loadEffects,
  loadEffect,
) where

import AgoraRegistry.Schema (EffectSchema)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map
import Optics.Core (view)
import System.Directory.Extra (listDirectory)

-- | A map from scripthashes to effect schemas available in the registry.
newtype EffectRegistry = EffectRegistry (Map ByteString EffectSchema)

-- | Loads and decodes effect schemas from a directory.
loadEffects :: FilePath -> IO EffectRegistry
loadEffects effectsDir = do
  effFiles <- listDirectory effectsDir <&> fmap (\f -> effectsDir <> "/" <> f)
  effects <- traverse loadEffect effFiles

  let effectMap =
        Map.fromList $ zip (view #scriptHash <$> effects) effects

  if Map.size effectMap /= length effects
    then fail "EffectRegistry contains multiple schemas for the same script hash."
    else pure $ EffectRegistry effectMap

-- | Loads and decodes an effect schema from a file.
loadEffect ::
  FilePath ->
  IO EffectSchema
loadEffect fp =
  Aeson.eitherDecodeFileStrict fp >>= \case
    Left err ->
      fail $
        mconcat
          [ "Could not load EffectSchema from file: "
          , fp
          , "\nError: "
          , err
          ]
    Right x -> pure x
