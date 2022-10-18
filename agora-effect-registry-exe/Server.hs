{- |
Module     : Server
Maintainer : michal@mlabs.city
Description: Provides server starting functions.

Provides server starting functions.
-}

module Server (
  server,
  app,
) where

import Api (API, api)
import AppM (AppM, encodeDatum, runApp)
import EffectRegistry (EffectRegistry)
import Servant (
  HasServer (ServerT),
  hoistServer,
  serveDirectoryFileServer,
  type (:<|>) ((:<|>)),
 )
import Servant.Server (Application, serve)

server :: FilePath -> ServerT API AppM
server effectsDir =
  -- must match path accepted by the api
  serveDirectoryFileServer effectsDir
    :<|> encodeDatum

app :: FilePath -> EffectRegistry -> Application
app effectsDir effects =
  serve api $
    hoistServer api (runApp effects) (server effectsDir)
