module AgoraRegistry.Schema (
  Effect (..),
  Schema (..),
  Metadata (..),
) where

import Data.Aeson ( (.:), FromJSON(parseJSON) )
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson

import qualified Data.Text as Text
import qualified Data.ByteString.UTF8 as UTF8 (fromString)
import qualified Data.ByteString.Base16 as Base16 (decodeBase16)
import Data.Text (Text)
import PlutusLedgerApi.V2 qualified as Plutus
import Data.Either (isRight)
import GHC.Generics (Generic)

data Metadata = Metadata
  { name :: Text
  , description :: Text
  } deriving stock (Generic)

instance Aeson.FromJSON Metadata


data Effect = Effect
  { meta :: Metadata
  , scriptHash :: Plutus.ScriptHash
  , datumSchema :: Schema
  }

instance Aeson.FromJSON Effect where
  parseJSON = Aeson.withObject "Effect" $ \o -> do
    meta <- o .: "meta"
    scriptHash <- parseHash28 =<< ( o .: "scriptHas")
    datumSchema <- o .: "datumSchema"
    pure $ Effect meta scriptHash datumSchema

data Schema = Schema
  { meta :: Metadata
  , schema :: DatumSchema
  }

instance Aeson.FromJSON Schema where
  parseJSON v  = flip (Aeson.withObject "Schema") v $ \o -> do
    meta <- o .: "meta"
    schema <- parseJSON v
    pure $ Schema meta schema

parseHash28 :: String -> Aeson.Parser Plutus.ScriptHash
parseHash28 s = Plutus.ScriptHash <$> do
  bts <- either (fail . Text.unpack) pure  (Base16.decodeBase16 (UTF8.fromString s ))
  pure $ Plutus.toBuiltin bts



data DatumSchema
  = ListSchema [Schema]
  | ConstrSchema [(Integer, [Schema])]
  | OneOfSchema [Schema]
  | MapSchema [(Schema, Schema)]
  | IntegerSchema
  | ByteStringDatum
  | PlutusSchema PlutusTypeSchema

instance Aeson.FromJSON DatumSchema where
  parseJSON v = flip (Aeson.withObject "Effect") v $ \o -> do
    schemaType :: String <- o .: "type"
    case schemaType of
      "list" -> ListSchema <$> o .: "elements"
      "constr" -> undefined --ConstrSchema <$> o .: "items"
      "oneOf" -> OneOfSchema <$> o .: "options"
      "map" -> undefined -- MapSchema <$> o .: "items"
      "integer" -> pure IntegerSchema
      "bytes" -> pure ByteStringDatum
      _ -> PlutusSchema <$> parseJSON v


data PlutusTypeSchema
  = AddressSchema
  | ValueSchema
  | CredentialSchema
  | AssetClassSchema
  | Hash32Schema
  | Hash28Schema


instance Aeson.FromJSON PlutusTypeSchema where
  parseJSON = undefined

validateJsonDatum' :: Schema -> Aeson.Value -> Bool
validateJsonDatum' s = isRight . validateJsonDatum s

validateJsonDatum :: Schema -> Aeson.Value -> Either String Plutus.Data
validateJsonDatum = undefined
