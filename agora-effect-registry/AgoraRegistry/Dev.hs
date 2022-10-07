module AgoraRegistry.Dev (
  getFixture,
  parseFixture,
) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as B
import AgoraRegistry.Schema (EffectSchema)

------------------------------------

getFixture :: IO B.ByteString
getFixture = B.readFile "./effects/TestShapedList.json"

parseFixture :: IO (Either String EffectSchema)
parseFixture = Aeson.eitherDecode <$> getFixture
