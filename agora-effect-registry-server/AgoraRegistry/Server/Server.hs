{- |
Module     : AgoraRegistry.Server.Server
Maintainer : michal@mlabs.city
Description: Provides server starting functions.

Provides server starting functions.
-}
module AgoraRegistry.Server.Server (
  app,
  runServer,
) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Function ((&))
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors (
  CorsResourcePolicy (corsRequestHeaders),
  cors,
  simpleCorsResourcePolicy,
 )
import Optics.Core (view)
import Prettyprinter (
  Pretty (pretty),
  defaultLayoutOptions,
  hsep,
  layoutPretty,
  viaShow,
 )
import Prettyprinter.Render.String (renderString)
import Servant (
  HasServer (ServerT),
  hoistServer,
  serve,
  serveDirectoryFileServer,
 )
import Servant.API ((:<|>) (..))
import Servant.Server (Application)
import Text.Printf (printf)

import AgoraRegistry.Server.Api (API, api)
import AgoraRegistry.Server.AppM (AppM, encodeDatum, registryInfo, runApp)
import AgoraRegistry.Server.EffectRegistry (EffectRegistry)
import AgoraRegistry.Server.Options (HttpServerOptions)

handler :: FilePath -> ServerT API AppM
handler effectsDir = do
  -- must match path accepted by the api
  serveDirectoryFileServer effectsDir
    :<|> encodeDatum
    :<|> registryInfo effectsDir

{- | Effect registry WAI application using AppM monad.

     @since 1.0.0
-}
app :: FilePath -> EffectRegistry -> Application
app effectsDir effects =
  serve api $ hoistServer api (runApp effects) (handler effectsDir)

{- | Run a Warp server providing effect registry services.

     @since 1.0.0
-}
runServer :: MonadIO m => FilePath -> EffectRegistry -> HttpServerOptions -> m ()
runServer effectsDir effects options = do
  let logger req status _maybeFileSize =
        putStrLn . renderString . layoutPretty defaultLayoutOptions $
          hsep
            [ "[info]"
            , viaShow $ Wai.requestMethod req
            , viaShow $ Wai.rawPathInfo req
            , "(" <> pretty (Http.statusCode status) <> ")"
            ]

      settings =
        Warp.defaultSettings
          & Warp.setPort (view #port options)
          & Warp.setLogger logger

      corsPolicy =
        simpleCorsResourcePolicy
          { -- NOTE: Webpack dev server requires this for CORS workaround.
            corsRequestHeaders = "content-type" : corsRequestHeaders simpleCorsResourcePolicy
          }

      corsMiddleware = cors . const $ Just corsPolicy

  liftIO $ printf "[info] Running agora effect registry server on :%d\n" (Warp.getPort settings)

  app effectsDir effects
    & (if view #enableCorsMiddleware options then corsMiddleware else id)
    & Warp.runSettings settings
    & liftIO
