module ApiTests (runApiTests) where

import Test.Hspec (Spec, describe, it, runIO)

import qualified Data.Aeson as Aeson
import AgoraRegistry.Server.EffectRegistry (loadEffects)
import AgoraRegistry.Server.Server (app)
import Test.Hspec.Wai (get, shouldRespondWith, with, ResponseMatcher (matchBody), MatchBody (MatchBody), Body)
import System.Directory.Extra (listDirectory)
import Control.Monad (forM_)
import Data.ByteString.UTF8 (fromString)
import Network.HTTP.Types.Header (Header)
import AgoraRegistry.Schema (EffectSchema)
import System.FilePath ((</>))

effectsDir :: FilePath
effectsDir = "./effects"

runApiTests :: Spec
runApiTests = do
  registry <- runIO $ loadEffects effectsDir
  with (pure $ app effectsDir registry) $ do
    describe "GET /effects" $ do
      it "should return valid response" $ do
        get "/effects" `shouldRespondWith` 200
      describe "should be able to get all existing effects" $ do
        effFiles <- runIO $ listDirectory effectsDir
        forM_ effFiles $ \effFile -> do
          bodyMatcher <- runIO $ getEffectBodyMatcher effFile
          it ("should be able to get and decode " <> effFile) $
            get ("/effects/" <> fromString effFile) `shouldRespondWith` 200 {matchBody = MatchBody bodyMatcher}
      it "should return 404 on not existing path" $
        get "/effects/nothing" `shouldRespondWith` 404

getEffectBodyMatcher :: FilePath -> IO ([Network.HTTP.Types.Header.Header] -> Body -> Maybe String)
getEffectBodyMatcher fp = do
  Right (effSchema :: EffectSchema) <- Aeson.eitherDecodeFileStrict' (effectsDir </> fp)
  pure $ \_ body -> case Aeson.eitherDecode body of
    Left err -> Just $ "Failed to decode effect schema: " <> err
    Right schema -> if schema == effSchema
      then Nothing
      else Just $ "Expected: " <> show effSchema <> ""
