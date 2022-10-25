module Main (main) where

import AgoraRegistry.Server.EffectRegistry (loadEffects)
import AgoraRegistry.Server.Options (Options (HttpServerOption), parseOptions)
import AgoraRegistry.Server.Server (runServer)

-- | Paths from which the server loads effect schemas.
effectsDir :: FilePath
effectsDir = "./effects"

main :: IO ()
main = do
  HttpServerOption opts <- parseOptions
  registry <- loadEffects effectsDir
  putStrLn "Effect schemas loaded."
  runServer effectsDir registry opts
