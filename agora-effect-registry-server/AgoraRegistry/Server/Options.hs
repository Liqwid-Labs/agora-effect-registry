{-# LANGUAGE TemplateHaskell #-}

{- | Module     : AgoraRegistry.Server.Options
     Maintainer : michal@mlabs.city
     Description: Command line options for 'agora-effect-registry-server'.

     Command line options for 'agora-effect-registry-server'.
-}
module AgoraRegistry.Server.Options (
  Options (..),
  HttpServerOptions (..),
  RegistryInfo (..),
  SchemaInfo (..),
) where

import AgoraRegistry.Server.Types (EffectScriptHash)
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import GHC.Generics qualified as GHC
import Network.Wai.Handler.Warp qualified as Warp
import Optics.TH (makeFieldLabelsNoPrefix)

{- | Information about hosted effect schema.

     @since 1.0.0
-}
data SchemaInfo = SchemaInfo
  { name :: Text
  , scriptHash :: EffectScriptHash
  }
  deriving anyclass
    ( -- | @since 1.0.0
      Aeson.ToJSON
    , -- | @since 1.0.0
      Aeson.FromJSON
    )
  deriving stock
    ( -- | @since 1.0.0
      Show
    , -- | @since 1.0.0
      Eq
    , -- | @since 1.0.0
      GHC.Generic
    )

{- | Information about the effect registry.

     @since 1.0.0
-}
data RegistryInfo = RegistryInfo
  { revision :: Text
  , hostedEffectDatumSchemas :: [SchemaInfo]
  }
  deriving anyclass
    ( -- | @since 1.0.0
      Aeson.ToJSON
    , -- | @since 1.0.0
      Aeson.FromJSON
    )
  deriving stock
    ( -- | @since 1.0.0
      Show
    , -- | @since 1.0.0
      Eq
    , -- | @since 1.0.0
      GHC.Generic
    )

{- | Command line options for the registry service.

     @since 1.0.0
-}
newtype Options
  = HttpServerOption HttpServerOptions
  -- StdIoOption -- TODO: add option for direct cli usage
  deriving stock
    ( -- | @since 1.0.0
      Show
    , -- | @since 1.0.0
      Eq
    )

-- | @since 1.0.0
data HttpServerOptions = HttpServerOptions
  { port :: Warp.Port
  -- ^ Which port to listen on.
  , enableCorsMiddleware :: Bool
  -- ^ Should we enable CORS middleware for debugging purposes?
  }
  deriving stock
    ( -- | @since 1.0.0
      Show
    , -- | @since 1.0.0
      Eq
    )

----------------------------------------
-- Field Labels

-- | @since 1.0.0
makeFieldLabelsNoPrefix ''HttpServerOptions
