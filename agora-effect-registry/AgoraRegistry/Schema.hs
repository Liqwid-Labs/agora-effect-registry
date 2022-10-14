{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module AgoraRegistry.Schema (
  EffectSchema (..),
  Schema,
  Schema' (..),
  DatumSchema (..),
  Metadata (..),
  PlutusTypeSchema (..),
  plutusTypeSchemaName,
  schemaName,
) where

import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics (Generic)
import Optics.TH (makeFieldLabelsNoPrefix)

import AgoraRegistry.Parsing (parseHex')

data Metadata = Metadata
  { name :: Text
  , description :: Text
  }
  deriving stock (Show, Generic)

makeFieldLabelsNoPrefix ''Metadata

instance Aeson.FromJSON Metadata

data Schema' a = Schema
  { meta :: Maybe Metadata
  , schema :: a
  }
  deriving stock (Show)

makeFieldLabelsNoPrefix ''Schema'

instance Aeson.FromJSON a => Aeson.FromJSON (Schema' a) where
  parseJSON v = flip (Aeson.withObject "Schema") v $ \o -> do
    meta <- o .:? "meta"
    schema <- Aeson.parseJSON v
    pure $ Schema meta schema

data PlutusTypeSchema
  = AddressSchema
  | ValueSchema
  | CredentialSchema
  | AssetClassSchema
  | Hash32Schema
  | Hash28Schema
  deriving stock (Show)

plutusTypeSchemaName :: PlutusTypeSchema -> String
plutusTypeSchemaName = \case
  AddressSchema -> "plutus/Address"
  ValueSchema -> "plutus/Value"
  CredentialSchema -> "plutus/Credential"
  AssetClassSchema -> "plutus/AssetClass"
  Hash32Schema -> "plutus/Hash32"
  Hash28Schema -> "plutus/Hash28"

instance Aeson.FromJSON PlutusTypeSchema where
  parseJSON v = flip (Aeson.withObject "PlutusTypeSchema") v $ \o -> do
    schemaType :: Text <- o .: "type"
    maybe (fail "Unknown schema type.") pure $
      case schemaType of
        "plutus/Address" -> Just AddressSchema
        "plutus/Value" -> Just ValueSchema
        "plutus/Credential" -> Just CredentialSchema
        "plutus/AssetClass" -> Just AssetClassSchema
        "plutus/Hash32" -> Just Hash32Schema
        "plutus/Hash28" -> Just Hash28Schema
        _ -> Nothing

data DatumSchema
  = ListSchema Schema
  | ShapedListSchema (NonEmpty Schema)
  | ConstrSchema Integer (NonEmpty Schema)
  | OneOfSchema (NonEmpty Schema)
  | MapSchema Schema Schema
  | IntegerSchema
  | ByteStringSchema
  | PlutusSchema PlutusTypeSchema
  deriving stock (Show)

type Schema = Schema' DatumSchema

makeFieldLabelsNoPrefix ''DatumSchema

instance Aeson.FromJSON DatumSchema where
  parseJSON v = flip (Aeson.withObject "DatumSchema") v $ \o -> do
    schemaType :: Text <- o .: "type"
    case schemaType of
      "list" -> ListSchema <$> o .: "elements"
      "shaped_list" -> ShapedListSchema <$> o .: "elements"
      "constr" -> ConstrSchema <$> o .: "tag" <*> o .: "fields"
      "oneOf" -> OneOfSchema <$> o .: "options"
      "map" -> MapSchema <$> o .: "keys" <*> o .: "values"
      "integer" -> pure IntegerSchema
      "bytes" -> pure ByteStringSchema
      _ -> PlutusSchema <$> Aeson.parseJSON v

schemaName :: DatumSchema -> String
schemaName = \case
  ListSchema _ -> "list"
  ShapedListSchema _ -> "shaped_list"
  ConstrSchema _ _ -> "constr"
  OneOfSchema _ -> "oneOf"
  MapSchema _ _ -> "map"
  IntegerSchema -> "integer"
  ByteStringSchema -> "bytes"
  PlutusSchema ps -> plutusTypeSchemaName ps

data EffectSchema = EffectSchema
  { metadata :: Metadata
  , scriptHash :: ByteString
  , datumSchema :: Schema
  }
  deriving stock (Show)

makeFieldLabelsNoPrefix ''EffectSchema

instance Aeson.FromJSON EffectSchema where
  parseJSON = Aeson.withObject "EffectSchema" $ \o -> do
    meta <- o .: "meta"
    scriptHash <- parseHex' 28 =<< o .: "scriptHash"
    datumSchema <- o .: "datumSchema"
    pure $ EffectSchema meta scriptHash datumSchema
