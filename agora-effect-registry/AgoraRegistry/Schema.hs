{-# LANGUAGE TemplateHaskell #-}

{- |
Module     : AgoraRegistry.Schema
Maintainer : michal@mlabs.city
Description: Type definitions for effect datum schema

Contains types and instances necessary for decoding
an Effect metadata schema from JSON.
-}
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

import AgoraRegistry.Parsing (parseHex')
import Data.Aeson (KeyValue ((.=)), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Optics.Core (view)
import Optics.TH (makeFieldLabelsNoPrefix)

{- | Provides additional description for schemas.

     @since 0.1.0
-}
data Metadata = Metadata
  { name :: Text
  -- ^ A name for given effect datum part.
  , description :: Text
  -- ^ A description of an effect datum part.
  }
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Show
    , -- | @since 0.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 0.1.0
      Aeson.FromJSON
    , -- | @since 0.1.0
      Aeson.ToJSON
    )

-- | @since 0.1.0
makeFieldLabelsNoPrefix ''Metadata

{- | An auxilliary type aggregating a schema and its metadata.

     @since 0.1.0
-}
data Schema' a = Schema
  { meta :: Maybe Metadata
  -- ^ An optional description of a schema piece.
  , schema :: a
  -- ^ The actual schema.
  }
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Show
    )

-- | @since 0.1.0
makeFieldLabelsNoPrefix ''Schema'

-- | @since 0.1.0
instance Aeson.FromJSON a => Aeson.FromJSON (Schema' a) where
  parseJSON v = flip (Aeson.withObject "Schema") v $ \o -> do
    meta <- o .:? "meta"
    schema <- Aeson.parseJSON v
    pure $ Schema meta schema

{- | An enumeration of schema-supported plutus types.

     @since 0.1.0
-}
data PlutusTypeSchema
  = AddressSchema
  | ValueSchema
  | CredentialSchema
  | Hash32Schema
  | Hash28Schema
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Show
    )

{- | Returns a name for given plutus type schema.

     @since 0.1.0
-}
plutusTypeSchemaName :: PlutusTypeSchema -> String
plutusTypeSchemaName = \case
  AddressSchema -> "plutus/Address"
  ValueSchema -> "plutus/Value"
  CredentialSchema -> "plutus/Credential"
  Hash32Schema -> "plutus/Hash32"
  Hash28Schema -> "plutus/Hash28"

-- | @since 0.1.0
instance Aeson.FromJSON PlutusTypeSchema where
  parseJSON v = flip (Aeson.withObject "PlutusTypeSchema") v $ \o -> do
    schemaType :: Text <- o .: "type"
    maybe (fail "Unknown schema type.") pure $
      case schemaType of
        "plutus/Address" -> Just AddressSchema
        "plutus/Value" -> Just ValueSchema
        "plutus/Credential" -> Just CredentialSchema
        "plutus/Hash32" -> Just Hash32Schema
        "plutus/Hash28" -> Just Hash28Schema
        _ -> Nothing

{- | An enumeration of schema-supported types which can encode an effect datum.

     NOTE: must be able to encode any value of type `Plutus.V2.Ledger.Api.Data`

     @since 0.1.0
-}
data DatumSchema
  = -- | Homogeneous list. Encodes Data's List ctor.
    ListSchema Schema
  | -- | Heterogenous list. Encodes Data's List ctor.
    ShapedListSchema (NonEmpty Schema)
  | -- | Constructor for records.
    ConstrSchema Integer [Schema]
  | -- | Will accept anything that adheres to any schema from a given set.
    OneOfSchema (NonEmpty Schema)
  | -- | Encodes Data's Map ctor.
    MapSchema Schema Schema
  | -- | Encodes Data's I ctor.
    IntegerSchema
  | -- | Encodes Data's B ctor.
    ByteStringSchema
  | -- | Encodes supported 'higher-level' plutus types.
    PlutusSchema PlutusTypeSchema
  | -- | Any plutus Data.
    AnySchema
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Show
    )

-- | Helper type alias.
type Schema = Schema' DatumSchema

-- | @since 0.1.0
makeFieldLabelsNoPrefix ''DatumSchema

-- | @since 0.1.0
instance Aeson.FromJSON DatumSchema where
  parseJSON v = flip (Aeson.withObject "DatumSchema") v $ \o -> do
    schemaType :: Text <- o .: "type"
    case schemaType of
      "list" -> ListSchema <$> o .: "elements"
      "shapedList" -> ShapedListSchema <$> o .: "elements"
      "constr" -> ConstrSchema <$> o .: "tag" <*> o .: "fields"
      "oneOf" -> OneOfSchema <$> o .: "options"
      "map" -> MapSchema <$> o .: "keys" <*> o .: "values"
      "integer" -> pure IntegerSchema
      "bytes" -> pure ByteStringSchema
      "any" -> pure AnySchema
      _ -> PlutusSchema <$> Aeson.parseJSON v

-- | Serialize a 'DatumSchema' into a Aeson object.
datumSchemaToObject :: DatumSchema -> Aeson.Object
datumSchemaToObject sch =
  KeyMap.insert
    "type"
    (Aeson.String $ Text.pack $ schemaName sch)
    (schemaInfoToObject sch)
  where
    object :: [Aeson.Pair] -> Aeson.Object
    object = KeyMap.fromList

    schemaInfoToObject :: DatumSchema -> Aeson.Object
    schemaInfoToObject (ListSchema es) = object ["elements" .= es]
    schemaInfoToObject (ShapedListSchema es) = object ["elements" .= es]
    schemaInfoToObject (ConstrSchema tag fields) =
      object ["tag" .= tag, "fields" .= fields]
    schemaInfoToObject (OneOfSchema opts) = object ["options" .= opts]
    schemaInfoToObject (MapSchema ks vs) = object ["keys" .= ks, "values" .= vs]
    schemaInfoToObject _ = KeyMap.empty

-- | @since 1.0.0
instance Aeson.ToJSON DatumSchema where
  toJSON = Aeson.Object . datumSchemaToObject

-- | @since 1.0.0
instance Aeson.ToJSON Schema where
  toJSON x =
    Aeson.Object $
      KeyMap.insert "meta" (Aeson.toJSON $ view #meta x) $
        datumSchemaToObject $
          view #schema x

{- | Returns a name for given datum schema.

    @since 0.1.0
-}
schemaName :: DatumSchema -> String
schemaName = \case
  ListSchema _ -> "list"
  ShapedListSchema _ -> "shapedList"
  ConstrSchema _ _ -> "constr"
  OneOfSchema _ -> "oneOf"
  MapSchema _ _ -> "map"
  IntegerSchema -> "integer"
  ByteStringSchema -> "bytes"
  PlutusSchema ps -> plutusTypeSchemaName ps
  AnySchema -> "any"

{- | Data type that holds the effect datum schema.

     @since 0.1.0
-}
data EffectSchema = EffectSchema
  { metadata :: Metadata
  -- ^ Description of the effect.
  , scriptHash :: ByteString
  -- ^ Hash of the effect validator script.
  , datumSchema :: Schema
  -- ^ Schema which all valid datums must abide.
  }
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Show
    )

-- | @since 0.1.0
makeFieldLabelsNoPrefix ''EffectSchema

-- | @since 0.1.0
instance Aeson.FromJSON EffectSchema where
  parseJSON = Aeson.withObject "EffectSchema" $ \o -> do
    meta <- o .: "meta"
    scriptHash <- parseHex' 28 =<< o .: "scriptHash"
    datumSchema <- o .: "datumSchema"
    pure $ EffectSchema meta scriptHash datumSchema

-- | @since 1.0.0
instance Aeson.ToJSON EffectSchema where
  toJSON x =
    Aeson.object
      [ "meta" .= view #metadata x
      , "scriptHash" .= Base16.encodeBase16 (view #scriptHash x)
      , "datumSchema" .= view #datumSchema x
      ]
