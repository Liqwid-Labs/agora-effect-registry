{- |
Module     : AgoraRegistry.Server.Server
Maintainer : michal@mlabs.city
Description: Provides server starting functions.

Provides server starting functions.
-}
module AgoraRegistry.Server.Server (
  server,
  app,
) where

import Servant (
  HasServer (ServerT),
  hoistServer,
  serveDirectoryFileServer,
  type (:<|>) ((:<|>)),
 )
import Servant.Server (Application, serve)

import AgoraRegistry.Server.Api (API, api)
import AgoraRegistry.Server.AppM (AppM, encodeDatum, runApp)
import AgoraRegistry.Server.EffectRegistry (EffectRegistry)

server :: FilePath -> ServerT API AppM
server effectsDir =
  -- must match path accepted by the api
  serveDirectoryFileServer effectsDir
    :<|> encodeDatum

app :: FilePath -> EffectRegistry -> Application
app effectsDir effects =
  serve api $
    hoistServer api (runApp effects) (server effectsDir)
