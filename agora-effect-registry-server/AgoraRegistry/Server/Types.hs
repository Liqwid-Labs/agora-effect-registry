{- |
Module     : AgoraRegistry.Server.Types
Maintainer : michal@mlabs.city
Description: Provides API types definition.

Types used by the service API.
-}
module AgoraRegistry.Server.Types (
  EffectDatum (EffectDatum),
  EffectScriptHash (EffectScriptHash),
) where

import qualified Data.Text.Lazy as Text (toStrict)
import Codec.Serialise (deserialiseOrFail, serialise)
import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseEither, (.=))
import Data.ByteString (ByteString, fromStrict)
import qualified Data.ByteString.Base16 as BS16 (encodeBase16)
import Data.ByteString.Lazy.Base16 (encodeBase16)
import Data.Either.Extra (mapLeft)
import qualified PlutusLedgerApi.V2 as Plutus
import Servant (
  FromHttpApiData (parseUrlPiece),
  ToHttpApiData (toUrlPiece),
 )

import AgoraRegistry.Parsing (parseHex, parseHex')

-- | API wrapper for representing and returning effect datums.
newtype EffectDatum = EffectDatum Plutus.Data
  deriving stock
    ( -- | @since 1.0.0
      Show
    , -- | @since 1.0.0
      Eq
    )

instance Aeson.ToJSON EffectDatum where
  toJSON (EffectDatum ed) =
    Aeson.object ["cborDatum" .= encodeBase16 (serialise ed)]

instance Aeson.FromJSON EffectDatum where
  parseJSON = Aeson.withObject "EffectDatum" $ \o -> do
    cborDatum <- o .: "cborDatum"
    datum <- either (fail . show) pure . deserialiseOrFail . fromStrict =<< parseHex cborDatum
    pure $ EffectDatum datum

-- | API wrapper for representing and accepting effect script hashes.
newtype EffectScriptHash = EffectScriptHash ByteString
  deriving stock
    ( -- | @since 1.0.0
      Show
    , -- | @since 1.0.0
      Eq
    )

instance Servant.ToHttpApiData EffectScriptHash where
  toUrlPiece (EffectScriptHash h) = BS16.encodeBase16 h

instance Servant.FromHttpApiData EffectScriptHash where
  parseUrlPiece t =
    EffectScriptHash
      <$> mapLeft (const "Invalid effect script hash.") (parseEither (parseHex' 28) t)

instance Aeson.ToJSON EffectScriptHash where
  toJSON (EffectScriptHash h) = Aeson.String $ Text.toStrict $ encodeBase16 (serialise h)

instance Aeson.FromJSON EffectScriptHash where
  parseJSON (Aeson.String t) = do
    datum <- either (fail . show) pure . deserialiseOrFail . fromStrict =<< parseHex t
    pure $ EffectScriptHash datum
  parseJSON _ = fail "Could not decode effect script hash from JSON."
