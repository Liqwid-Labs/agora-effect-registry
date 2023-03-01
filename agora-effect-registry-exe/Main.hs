module Main (main) where

import AgoraRegistry.Schema (EffectSchema)
import AgoraRegistry.Server.EffectRegistry (loadEffect, loadEffects)
import AgoraRegistry.Server.Options (HttpServerOptions)
import AgoraRegistry.Server.Server (runServer)
import Control.Monad (unless)
import Control.Monad.Except (catchError)
import Data.Aeson qualified as Aeson
import Data.Foldable (traverse_)
import Options (Commands (Serve, Verify), parseOptions)

main :: IO ()
main =
  parseOptions >>= \case
    Serve effectDir serverOpts -> serve effectDir serverOpts
    Verify paths -> verify paths

serve :: FilePath -> HttpServerOptions -> IO ()
serve effectsDir serverOpts = do
  registry <- loadEffects effectsDir
  putStrLn "Effect schemas loaded."
  runServer effectsDir registry serverOpts

verify :: [FilePath] -> IO ()
verify =
  traverse_
    ( \path -> catchError (verifySingle path) $ \err ->
        putStrLn $
          "Error while verifying "
            <> show path
            <> ": "
            <> show err
    )
  where
    verifySingle :: FilePath -> IO ()
    verifySingle path = do
      schema <- loadEffect path

      case Aeson.decode @EffectSchema $ Aeson.encode schema of
        Nothing -> fail "round-trip failure"
        Just schema' ->
          unless (schema' == schema) $
            fail "schema should be identical after round-trip"
