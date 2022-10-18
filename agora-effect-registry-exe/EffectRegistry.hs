{- |
Module     : Api
Maintainer : michal@mlabs.city
Description: Defines EffectRegistry types and functions.

Defines EffectRegistry types and functions.
-}
module EffectRegistry (
  EffectRegistry (EffectRegistry),
  loadEffects,
  loadEffect,
) where

import AgoraRegistry.Schema (EffectSchema)
import qualified Data.Aeson as Aeson
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as Map
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
    Right effs -> pure $ EffectRegistry $ Map.fromList effs

-- | Loads and decodes an effect schema from a file.
loadEffect ::
  FilePath ->
  IO (Either (FilePath, String) (ByteString, EffectSchema))
loadEffect fp =
  Aeson.eitherDecodeFileStrict' fp
    <&> bimap
      (fp,)
      (\eff -> (view #scriptHash eff, eff))
