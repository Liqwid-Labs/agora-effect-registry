{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module FixtureTests (runFixtureTests) where

import Control.Arrow (first)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Bifunctor (bimap)
import Data.Either (fromRight, isLeft, isRight)
import Data.Foldable (for_, traverse_)
import Data.Functor ((<&>))
import Data.List (isPrefixOf)
import Data.Traversable (for)
import GHC.Generics (Generic)
import Optics.Core (view)
import Optics.TH (makeFieldLabelsNoPrefix)
import System.Directory.Extra (listDirectory)
import System.FilePath (takeBaseName, (</>))
import Test.Hspec (Spec, describe, it, parallel, runIO, shouldSatisfy)

import AgoraRegistry.DatumValidation (validateEffectDatum)

schemaFixturesPath :: FilePath
schemaFixturesPath = "./test/fixtures/schemas"

validDatumFixturesPath :: FilePath
validDatumFixturesPath = "./test/fixtures/json-datums/valid"

invalidDatumFixturesPath :: FilePath
invalidDatumFixturesPath = "./test/fixtures/json-datums/invalid"

loadFixtures :: FilePath -> IO [(FilePath, Aeson.Value)]
loadFixtures dir = do
  files <- listDirectory dir <&> fmap (dir </>)
  fixturesE <- traverse loadFixture files
  case sequence fixturesE of
    Left (fp, err) ->
      fail $
        concat
          [ "Could not decode json fixture from from file: "
          , fp
          , "\nError: "
          , err
          ]
    Right xs -> pure xs
  where
    loadFixture ::
      FilePath ->
      IO (Either (FilePath, String) (FilePath, Aeson.Value))
    loadFixture fp =
      Aeson.eitherDecodeFileStrict' fp
        <&> bimap
          (fp,)
          (\eff -> (fp, eff))

data FixtureTest = FixtureTest
  { name :: FilePath
  , jsonSchema :: Aeson.Value
  , validDatums :: [(FilePath, Aeson.Value)]
  , invalidDatums :: [(FilePath, Aeson.Value)]
  }
  deriving stock (Show, Generic)
makeFieldLabelsNoPrefix ''FixtureTest

prepareFixtureTests :: IO [FixtureTest]
prepareFixtureTests = do
  schemas <- loadFixtures schemaFixturesPath
  valid <- loadFixtures validDatumFixturesPath
  invalid <- loadFixtures invalidDatumFixturesPath
  for schemas $ \(schemaPath, schema) -> do
    let schemaName = takeBaseName schemaPath
    let validDatums =
          first takeBaseName
            <$> filter ((schemaName `isPrefixOf`) . takeBaseName . fst) valid
    let invalidDatums =
          first takeBaseName
            <$> filter ((schemaName `isPrefixOf`) . takeBaseName . fst) invalid
    pure $ FixtureTest schemaName schema validDatums invalidDatums

runFixtureTest :: FixtureTest -> Spec
runFixtureTest test =
  describe (view #name test) $
    parallel $ do
      let schema' = Aeson.parseEither Aeson.parseJSON (view #jsonSchema test)
      it "Should decode the EffectSchema from JSON" $ do
        schema' `shouldSatisfy` isRight
      let effectSchema = fromRight undefined schema'
      for_ (view #validDatums test) $ \(n, datum) ->
        it ("Should parse and succesfully encode as Plutus data - " <> show n) $ do
          let result = validateEffectDatum effectSchema datum
          result `shouldSatisfy` isRight
      for_ (view #invalidDatums test) $ \(n, datum) ->
        it ("Should parse and fail to encode as Plutus data - " <> show n) $ do
          let result = validateEffectDatum effectSchema datum
          result `shouldSatisfy` isLeft

runFixtureTests :: Spec
runFixtureTests =
  describe "Schema FromJSON and datum encoding fixture tests" $
    runIO prepareFixtureTests >>= traverse_ runFixtureTest
