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
  parseOptions,
) where

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified GHC.Generics as GHC
import qualified Network.Wai.Handler.Warp as Warp
import Optics.TH (makeFieldLabelsNoPrefix)
import Options.Applicative ((<**>))
import qualified Options.Applicative as Opt

import AgoraRegistry.Server.Types (EffectScriptHash)

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

serverOpt :: Opt.Parser HttpServerOptions
serverOpt =
  HttpServerOptions
    <$> Opt.option
      Opt.auto
      ( Opt.long "port"
          <> Opt.short 'p'
          <> Opt.metavar "PORT"
          <> Opt.value 3838
          <> Opt.help "The port to run the registry server on."
      )
    <*> Opt.switch
      ( Opt.long "enable-cors-middleware"
          <> Opt.short 'c'
          <> Opt.help
            ( unwords
                [ "Enable CORS middleware."
                , "This is usually required for some local servers."
                , "For security reasons, this should be disabled in production."
                ]
            )
      )

opt :: Opt.Parser Options
opt = HttpServerOption <$> serverOpt

{- | Parse 'Options' from the command line arguments.

     @since 1.0.0
-}
parseOptions :: IO Options
parseOptions = Opt.execParser p
  where
    p =
      Opt.info
        (opt <**> Opt.helper)
        ( Opt.fullDesc
            <> Opt.progDesc "The Agora Effect Registry service."
        )

----------------------------------------
-- Field Labels

-- | @since 1.0.0
makeFieldLabelsNoPrefix ''Options

-- | @since 1.0.0
makeFieldLabelsNoPrefix ''HttpServerOptions
