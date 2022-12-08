{- |
Module     : AgoraRegistry.Server.Api
aintainer : michal@mlabs.city
Description: Provides API definition.

Provides API definition.
-}
module AgoraRegistry.Server.Api (
  API,
  api,
) where

import Data.Aeson qualified as Aeson
import Data.Proxy (Proxy (Proxy))
import Servant (
  Capture,
  JSON,
  Post,
  Raw,
  ReqBody,
  type (:<|>),
  type (:>),
 )

import AgoraRegistry.Server.Options (RegistryInfo)
import AgoraRegistry.Server.Types (EffectDatum, EffectScriptHash)
import Servant.API (Get)

type API =
  "effects" :> Raw
    :<|> ( "encodeEffectDatum"
            :> Capture "scriptHash" EffectScriptHash
            :> ReqBody '[JSON] Aeson.Value
            :> Post '[JSON] EffectDatum
         )
    :<|> "info"
      :> Get '[JSON] RegistryInfo

api :: Data.Proxy.Proxy API
api = Data.Proxy.Proxy
