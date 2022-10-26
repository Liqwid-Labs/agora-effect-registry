module ApiTests (runApiTests) where

import Test.Hspec (Spec, describe, it, runIO)

import AgoraRegistry.Schema (EffectSchema)
import AgoraRegistry.Server.EffectRegistry (loadEffects)
import AgoraRegistry.Server.Server (app)
import AgoraRegistry.Server.Types (EffectScriptHash (EffectScriptHash))
import Control.Monad (forM_)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.UTF8 (ByteString, fromString)
import Data.Foldable (for_)
import Data.Kind (Type)
import Data.Text.Encoding (encodeUtf8)
import qualified FixtureTests
import Network.HTTP.Types (hContentType, methodPost)
import Network.HTTP.Types.Header (Header)
import Network.Wai.Test (SResponse)
import Optics.Core (view)
import Servant (ToHttpApiData (toUrlPiece))
import System.Directory.Extra (listDirectory)
import System.FilePath ((</>))
import Test.Hspec.Wai (Body, MatchBody (MatchBody), ResponseMatcher (matchBody), WaiSession, get, post, request, shouldRespondWith, with)

effectsDir :: FilePath
effectsDir = "./effects"

runApiTests :: Spec
runApiTests = do
  registry <- runIO $ loadEffects effectsDir
  with (pure $ app effectsDir registry) $ do
    getEffectTests
    encodeEffectDatumErrorTests
  encodeEffectFixtureSchemaTests
  where
    existingSchemaScriptHash :: FilePath -> IO ByteString
    existingSchemaScriptHash schemaDir = do
      effFile <- head <$> listDirectory schemaDir
      getEffectSchemaScriptHash (schemaDir </> effFile)

    getEffectSchemaScriptHash :: FilePath -> IO ByteString
    getEffectSchemaScriptHash schemaPath = do
      Right (effSchema :: EffectSchema) <- Aeson.eitherDecodeFileStrict' schemaPath
      pure $ view #scriptHash effSchema

    encodeEffectFixtureSchemaTests = describe "valid cases - POST /encodeEffectDatum/[scripthash]" $ do
      let schemaPath = FixtureTests.schemaFixturesPath
      registry' <- runIO $ loadEffects schemaPath
      with (pure $ app schemaPath registry') $ do
        tests <- runIO FixtureTests.prepareFixtureTests
        for_ tests $ \test -> do
          hash <- runIO $ EffectScriptHash <$> getEffectSchemaScriptHash (view #schemaPath test)
          for_ (view #validDatums test) $ \(fp, jsonDatum) -> do
            it ("should validate and encode " <> fp <> " content.") $ do
              let resp = postJson @Aeson.Value ("/encodeEffectDatum/" <> encodeUtf8 (toUrlPiece hash)) jsonDatum
              resp `shouldRespondWith` 200

    encodeEffectDatumErrorTests = describe "POST /encodeEffectDatum/[scripthash] - error cases" $ do
      hash <- runIO $ existingSchemaScriptHash effectsDir
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

postJson :: forall (a :: Type) (st :: Type). Aeson.ToJSON a => ByteString -> a -> WaiSession st SResponse
postJson route = request methodPost route [(hContentType, "application/json" :: ByteString)] . Aeson.encode
