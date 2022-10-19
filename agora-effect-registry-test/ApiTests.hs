module ApiTests (runApiTests) where

import Test.Hspec (Spec, describe, it, runIO)

import AgoraRegistry.Schema (EffectSchema)
import AgoraRegistry.Server.EffectRegistry (loadEffects)
import AgoraRegistry.Server.Server (app)
import Control.Monad (forM_)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.UTF8 (fromString, ByteString)
import Network.HTTP.Types.Header (Header)
import System.Directory.Extra (listDirectory)
import System.FilePath ((</>))
import Test.Hspec.Wai (Body, MatchBody (MatchBody), ResponseMatcher (matchBody), get, post, shouldRespondWith, with, request, WaiSession)
import Optics.Core (view)
import Network.HTTP.Types (methodPost, hContentType)
import Data.Kind (Type)
import Network.Wai.Test (SResponse)

effectsDir :: FilePath
effectsDir = "./effects"

runApiTests :: Spec
runApiTests = do
  registry <- runIO $ loadEffects effectsDir
  with (pure $ app effectsDir registry) $ do
    getEffectTests
    encodeEffectDatumErrorTests
  where
    existingSchemaScriptHash :: IO ByteString
    existingSchemaScriptHash = do
      effFiles <- listDirectory effectsDir
      Right (effSchema :: EffectSchema) <- Aeson.eitherDecodeFileStrict' (effectsDir </> head effFiles)
      pure $ view #scriptHash effSchema

    encodeEffectDatumErrorTests = describe "POST /encodeEffectDatum/[scripthash]" $ do
      hash <- runIO existingSchemaScriptHash
      it "should return 400 if url doesn't contain valid script hash" $ do
        post "/encodeEffectDatum/0000000000000000000000000000000000000000000000000000000000000001" "" `shouldRespondWith` 400
      it "should return 404 if the effect does not exist" $ do
        postJson' "/encodeEffectDatum/00000000000000000000000000000000000000000000000000000001" "{\"type\":\"integer\",\"value\":10}" `shouldRespondWith` 404
      it "should return 415 if content-type header not properly set" $ do
        post "/encodeEffectDatum/00000000000000000000000000000000000000000000000000000001" "json string" `shouldRespondWith` 415
      it "should return 400 if the body is not valid JSON" $ do
        post ("/encodeEffectDatum/" <> hash) "not json" `shouldRespondWith` 400
      it "should return 400 if the body is not valid for the effect" $ do
        postJson' ("/encodeEffectDatum/" <> hash) "{\"type\":\"integer\",\"value\":10}" `shouldRespondWith` 400

    getEffectTests = describe "GET /effects" $ do
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
    Right schema ->
      if schema == effSchema
        then Nothing
        else Just $ "Expected: " <> show effSchema <> ""

postJson' :: forall (st :: Type). ByteString -> ByteString -> WaiSession st SResponse
postJson' route = request methodPost route [(hContentType, "application/json" :: ByteString)] . BL.fromStrict

-- postJson :: forall (a :: Type) (st :: Type). Aeson.ToJSON a => ByteString -> a -> WaiSession st SResponse
-- postJson route = request methodPost route [(hContentType, "application/json" :: ByteString)] . Aeson.encode
