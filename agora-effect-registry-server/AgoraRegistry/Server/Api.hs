{- |
Module     : AgoraRegistry.Server.Api
Maintainer : michal@mlabs.city
Description: Provides API definition.

Provides API definition.
-}
module AgoraRegistry.Server.Api (
  EffectDatum (EffectDatum),
  EffectScriptHash (EffectScriptHash),
  API,
  api,
) where

import AgoraRegistry.Parsing (parseHex')
import Codec.Serialise (serialise)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseEither, (.=))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Base16 (encodeBase16)
import Data.Either.Extra (mapLeft)
import Data.Proxy (Proxy (Proxy))
import qualified PlutusLedgerApi.V2 as Plutus
import Servant (
  Capture,
  FromHttpApiData (parseUrlPiece),
  JSON,
  Post,
  Raw,
  ReqBody,
  type (:<|>),
  type (:>),
 )

instance Aeson.ToJSON EffectDatum where
  toJSON (EffectDatum ed) =
    Aeson.object ["cborDatum" .= encodeBase16 (serialise ed)]

newtype EffectScriptHash = EffectScriptHash ByteString
  deriving stock (Eq)

instance FromHttpApiData EffectScriptHash where
  parseUrlPiece t =
    EffectScriptHash
      <$> mapLeft (const "Invalid effect script hash.") (parseEither (parseHex' 28) t)

newtype EffectDatum = EffectDatum Plutus.Data

type API =
  "effects" :> Raw
    :<|> ( "encodeEffectDatum"
            :> Capture "scriptHash" EffectScriptHash
            :> ReqBody '[JSON] Aeson.Value
            :> Post '[JSON] EffectDatum
         )

api :: Data.Proxy.Proxy API
api = Data.Proxy.Proxy
