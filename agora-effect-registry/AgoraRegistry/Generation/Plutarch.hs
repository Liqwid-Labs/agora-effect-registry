{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module AgoraRegistry.Generation.Plutarch (PHasDatumSchema (..)) where

import AgoraRegistry.Schema (
  DatumSchema (
    AnySchema,
    ByteStringSchema,
    ConstrSchema,
    IntegerSchema,
    ListSchema,
    MapSchema,
    OneOfSchema,
    PlutusSchema,
    ShapedListSchema
  ),
  Metadata (..),
  PlutusTypeSchema (
    AddressSchema,
    CredentialSchema,
    Hash28Schema,
    Hash32Schema,
    ValueSchema
  ),
  Schema,
  Schema' (Schema),
 )
import Data.Kind (Constraint)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Proxy (Proxy (Proxy))
import Data.Text qualified as Text
import GHC.Natural (Natural)
import GHC.TypeLits (
  ErrorMessage (Text),
  KnownSymbol,
  TypeError,
  symbolVal,
 )
import Plutarch.Api.V1 (
  PAddress,
  PCredential,
  PCurrencySymbol,
  PDatum,
  PDatumHash,
  PMap,
  PMaybeData,
  PPOSIXTime,
  PTokenName,
  PTxId,
  PTxOutRef,
  PValue,
 )
import Plutarch.Api.V1.Scripts (PScriptHash)
import Plutarch.Extra.AssetClass (PAssetClassData)
import Plutarch.Extra.FixedDecimal (PFixedDecimal)
import Plutarch.Extra.Tagged (PTagged)
import Plutarch.Positive (PPositive)
import Plutarch.Prelude (
  PAsData,
  PBuiltinList,
  PByteString,
  PData,
  PDataRecord,
  PDataSum,
  PInteger,
  PIsData,
  PLabeledType (..),
  PRational,
  PSubtype,
  PType,
  PUnit,
  PlutusType (PInner),
 )

-- | @since 1.0.0
class PIsData a => PHasDatumSchema (a :: PType) where
  -- | The schema of a plutarch type.
  pdatumSchema :: DatumSchema
  default pdatumSchema :: (PHasDatumSchema (PInner a)) => DatumSchema
  pdatumSchema = pdatumSchema @(PInner a)

--------------------------------------------------------------------------------

-- * Orphan instances

instance PHasDatumSchema PData where
  pdatumSchema = AnySchema

instance PHasDatumSchema PByteString where
  pdatumSchema = ByteStringSchema

instance PHasDatumSchema PInteger where
  pdatumSchema = IntegerSchema

instance
  (PHasDatumSchema a, PIsData (PAsData a)) =>
  PHasDatumSchema (PAsData a)
  where
  pdatumSchema = pdatumSchema @a

instance
  (PHasDatumSchema a, PSubtype PData a) =>
  PHasDatumSchema (PBuiltinList a)
  where
  pdatumSchema = ListSchema $ pdatumSchemaNoMetadata @a

instance
  (PHasDatumSchema k, PHasDatumSchema v) =>
  PHasDatumSchema (PMap kg k v)
  where
  pdatumSchema =
    MapSchema
      (pdatumSchemaNoMetadata @k)
      (pdatumSchemaNoMetadata @v)

instance
  ( NonEmptyList xs
  , PDataRecordSchemaHelper xs
  ) =>
  PHasDatumSchema (PDataRecord xs)
  where
  pdatumSchema =
    ShapedListSchema $
      NonEmpty.fromList $
        pdataRecordSchema' @xs

instance
  (NonEmptyList xss, PDataSumSchemaHelper xss) =>
  PHasDatumSchema (PDataSum xss)
  where
  pdatumSchema =
    OneOfSchema $
      NonEmpty.fromList $
        pconstrSchemas' @xss 0

instance PHasDatumSchema PAddress where
  pdatumSchema = PlutusSchema AddressSchema

instance PHasDatumSchema (PValue kg ag) where
  pdatumSchema = PlutusSchema ValueSchema

instance PHasDatumSchema PCredential where
  pdatumSchema = PlutusSchema CredentialSchema

instance PHasDatumSchema a => PHasDatumSchema (PTagged tag a) where
  pdatumSchema = pdatumSchema @a

instance PHasDatumSchema PDatumHash where
  pdatumSchema = PlutusSchema Hash32Schema

instance PHasDatumSchema PScriptHash where
  pdatumSchema = PlutusSchema Hash28Schema

instance
  (PHasDatumSchema a) =>
  PHasDatumSchema (PMaybeData a)

instance PHasDatumSchema PPOSIXTime

instance PHasDatumSchema PDatum

instance PHasDatumSchema PTokenName

instance PHasDatumSchema PCurrencySymbol

instance PHasDatumSchema PAssetClassData

instance PHasDatumSchema (PFixedDecimal n)

instance PHasDatumSchema PPositive

instance PHasDatumSchema PRational where
  pdatumSchema =
    ShapedListSchema $
      NonEmpty.fromList $
        mkSchemaNoMetadata
          <$> [IntegerSchema, IntegerSchema]

instance PHasDatumSchema PTxId

instance PHasDatumSchema PTxOutRef

instance PHasDatumSchema PUnit where
  pdatumSchema = ConstrSchema 0 []

--------------------------------------------------------------------------------

-- * Helpers

mkSchemaNoMetadata :: DatumSchema -> Schema
mkSchemaNoMetadata = Schema Nothing

pdatumSchemaNoMetadata :: forall (a :: PType). PHasDatumSchema a => Schema
pdatumSchemaNoMetadata = mkSchemaNoMetadata $ pdatumSchema @a

class PDataSumSchemaHelper (xss :: [[PLabeledType]]) where
  pconstrSchemas' :: Natural -> [Schema]

instance PDataSumSchemaHelper '[] where
  pconstrSchemas' _ = []

instance
  (PDataRecordSchemaHelper x, PDataSumSchemaHelper xs) =>
  PDataSumSchemaHelper (x ': xs)
  where
  pconstrSchemas' idx =
    mkSchemaNoMetadata
      (ConstrSchema (fromIntegral idx) (pdataRecordSchema' @x))
      : pconstrSchemas' @xs (idx + 1)

type NonEmptyList :: [k] -> Constraint
type family NonEmptyList xs where
  NonEmptyList '[] =
    TypeError ('Text "Expected non-empty list")
  NonEmptyList _ = ()

class PDataRecordSchemaHelper (xs :: [PLabeledType]) where
  pdataRecordSchema' :: [Schema]

instance PDataRecordSchemaHelper '[] where
  pdataRecordSchema' = []

instance
  ( PDataRecordSchemaHelper xs
  , KnownSymbol s
  , PHasDatumSchema t
  ) =>
  PDataRecordSchemaHelper ((s ':= t) ': xs)
  where
  pdataRecordSchema' =
    Schema (Just metadata) (pdatumSchema @t)
      : pdataRecordSchema' @xs
    where
      metadata =
        Metadata
          { name =
              Text.pack $
                symbolVal $
                  Proxy @s
          , description = "data record field"
          }
