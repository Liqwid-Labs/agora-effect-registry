module Main (main) where

import EffectRegistry (loadEffects)
import Network.Wai.Handler.Warp (run)
import Server (app)

-- | Paths from which the server loads effect schemas.
effectsDir :: FilePath
effectsDir = "./effects"

main :: IO ()
main = do
  registry <- loadEffects effectsDir
  putStrLn "Effect schemas loaded."
  run 9999 (app effectsDir registry)
