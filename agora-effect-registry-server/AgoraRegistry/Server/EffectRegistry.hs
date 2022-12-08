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
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Data.List (nub)
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
  effectsE <- traverse loadEffect effFiles
  case sequence effectsE of
    Left (fp, err) ->
      fail $
        concat
          [ "Could not load EffectSchema from file: "
          , fp
          , "\nError: "
          , err
          ]
    Right effs -> do
      let scripthashes = fst <$> effs
      if length scripthashes /= length (nub scripthashes)
        then fail "EffectRegistry contains multiple schemas for the same script hash."
        else pure $ EffectRegistry $ Map.fromList effs

-- | Loads and decodes an effect schema from a file.
loadEffect ::
  FilePath ->
  IO (Either (FilePath, String) (ByteString, EffectSchema))
loadEffect fp =
  Aeson.eitherDecodeFileStrict' fp
    <&> bimap
      (fp,)
      (\eff -> (view #scriptHash eff, eff))
