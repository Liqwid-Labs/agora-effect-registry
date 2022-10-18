module Main (main) where

import AgoraRegistry.Server.EffectRegistry (loadEffects)
import AgoraRegistry.Server.Server (app)
import Network.Wai.Handler.Warp (run)

-- | Paths from which the server loads effect schemas.
effectsDir :: FilePath
effectsDir = "./effects"

main :: IO ()
main = do
  registry <- loadEffects effectsDir
  putStrLn "Effect schemas loaded."
  run 9999 (app effectsDir registry)
