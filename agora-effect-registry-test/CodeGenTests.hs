{-# LANGUAGE AllowAmbiguousTypes #-}

module CodeGenTests (codeGenTests) where

import AgoraRegistry.Generation.Plutarch (
  PHasDatumSchema (pdatumSchema),
 )
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
import Data.Data (Typeable)
import Data.Function (on)
import Data.List.NonEmpty qualified as NE
import Data.Proxy (Proxy (Proxy))
import Data.Typeable (typeRep)
import Plutarch.Api.V1 (
  AmountGuarantees (NoGuarantees),
  KeyGuarantees (Sorted),
  PCredential,
 )
import Plutarch.Api.V2 (PAddress, PDatum, PDatumHash, PScriptHash, PValue)
import Plutarch.Extra.IsData (PlutusTypeEnumData)
import Plutarch.Prelude
import Test.Hspec (Expectation, Spec, describe, it, parallel, shouldBe)

class PHasExpectedDatumchema (a :: PType) where
  pexpectedDatumSchema :: DatumSchema

class
  (Typeable a, PHasDatumSchema a, PHasExpectedDatumchema a) =>
  PTestableHasDatumSchemaType (a :: PType)

instance
  (Typeable a, PHasDatumSchema a, PHasExpectedDatumchema a) =>
  PTestableHasDatumSchemaType a

newtype TestableSchema = TestableSchema DatumSchema
  deriving newtype (Show)

-- Ignore the metadata cause we don't care.
-- TODO(Connor): parameterize DatumSchema on type level
instance Eq TestableSchema where
  (==) (TestableSchema l) (TestableSchema r) = datumSchemaEq l r
    where
      schemaEq :: Schema -> Schema -> Bool
      schemaEq (Schema _ l) (Schema _ r) =
        datumSchemaEq l r

      datumSchemaEq :: DatumSchema -> DatumSchema -> Bool
      datumSchemaEq (ListSchema l) (ListSchema r) = schemaEq l r
      datumSchemaEq (ShapedListSchema l) (ShapedListSchema r) =
        and $ NE.zipWith schemaEq l r
      datumSchemaEq (ConstrSchema il fl) (ConstrSchema ir fr) =
        il == ir && and (zipWith schemaEq fl fr)
      datumSchemaEq (OneOfSchema l) (OneOfSchema r) =
        and $ NE.zipWith schemaEq l r
      datumSchemaEq (MapSchema kl vl) (MapSchema kr vr) =
        schemaEq kl kr && schemaEq vl vr
      datumSchemaEq IntegerSchema IntegerSchema = True
      datumSchemaEq ByteStringSchema ByteStringSchema = True
      datumSchemaEq (PlutusSchema l) (PlutusSchema r) = l == r
      datumSchemaEq AnySchema AnySchema = True
      datumSchemaEq _ _ = False

hasCorrectSchema ::
  forall (a :: PType).
  PTestableHasDatumSchemaType a =>
  Expectation
hasCorrectSchema =
  (shouldBe `on` TestableSchema)
    (pexpectedDatumSchema @a)
    (pdatumSchema @a)

testCase ::
  forall (a :: PType).
  PTestableHasDatumSchemaType a =>
  Spec
testCase =
  let typeName = show $ typeRep $ Proxy @a
   in it (typeName <> " should have correct schema") $ hasCorrectSchema @a

wrapDatumSchema :: DatumSchema -> Schema
wrapDatumSchema = Schema Nothing

--------------------------------------------------------------------------------

codeGenTests :: Spec
codeGenTests =
  describe "Schema generation tests" $
    parallel $ do
      testCase @(PNewtypeOf PInteger)
      testCase @(PNewtypeOf PByteString)
      testCase @(PNewtypeOf PInteger)
      testCase @(PNewtypeOf (PNewtypeOf PInteger))
      testCase @(PNewtypeOf PUnit)
      testCase @POneConstructor
      testCase @PManyConstructors
      testCase @PEnumEncoded
      testCase @PListEncoded

--------------------------------------------------------------------------------

newtype PNewtypeOf (a :: PType) (s :: S)
  = PNewtypeOf (Term s a)
  deriving stock (Generic)
  deriving anyclass
    ( PlutusType
    , PIsData
    , PHasDatumSchema
    )

instance DerivePlutusType (PNewtypeOf a) where
  type DPTStrat _ = PlutusTypeNewtype

instance PHasExpectedDatumchema (PNewtypeOf PInteger) where
  pexpectedDatumSchema = IntegerSchema

instance PHasExpectedDatumchema (PNewtypeOf PByteString) where
  pexpectedDatumSchema = ByteStringSchema

instance PHasExpectedDatumchema (PNewtypeOf PUnit) where
  pexpectedDatumSchema = ConstrSchema 0 []

instance PHasExpectedDatumchema (PNewtypeOf (PNewtypeOf PInteger)) where
  pexpectedDatumSchema = IntegerSchema

--------------------------------------------------------------------------------

newtype POneConstructor (s :: S)
  = POneConstructor
      ( Term
          s
          ( PDataRecord
              '[ "a" ':= PInteger
               , "b" ':= PNewtypeOf PInteger
               , "c" ':= PNewtypeOf PByteString
               , "d" ':= PBuiltinList (PAsData PInteger)
               , "e" ':= PAddress
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass
    ( PlutusType
    , PIsData
    , PHasDatumSchema
    )

instance DerivePlutusType POneConstructor where
  type DPTStrat _ = PlutusTypeData

instance PHasExpectedDatumchema POneConstructor where
  pexpectedDatumSchema =
    ConstrSchema
      0
      $ wrapDatumSchema
        <$> [ IntegerSchema
            , IntegerSchema
            , ByteStringSchema
            , ListSchema $ wrapDatumSchema IntegerSchema
            , PlutusSchema AddressSchema
            ]

--------------------------------------------------------------------------------

data PManyConstructors (s :: S)
  = PConstructorA
      ( Term
          s
          ( PDataRecord
              '[ "aA" ':= PInteger
               , "bA" ':= POneConstructor
               ]
          )
      )
  | PConstructorB (Term s (PDataRecord '["aB" ':= PDatum]))
  | PConstructorC (Term s (PDataRecord '["aC" ':= PByteString]))
  deriving stock (Generic)
  deriving anyclass
    ( PlutusType
    , PIsData
    , PHasDatumSchema
    )

instance DerivePlutusType PManyConstructors where
  type DPTStrat _ = PlutusTypeData

instance PHasExpectedDatumchema PManyConstructors where
  pexpectedDatumSchema =
    OneOfSchema $
      NE.fromList $
        wrapDatumSchema
          <$> [ ConstrSchema
                  0
                  $ wrapDatumSchema
                    <$> [ IntegerSchema
                        , pexpectedDatumSchema @POneConstructor
                        ]
              , ConstrSchema 1 [wrapDatumSchema AnySchema]
              , ConstrSchema 2 [wrapDatumSchema ByteStringSchema]
              ]

--------------------------------------------------------------------------------

data PEnumEncoded (s :: S)
  = PEnumA
  | PEnumB
  | PEnumC
  deriving stock
    ( Generic
    , Bounded
    , Enum
    )
  deriving anyclass
    ( PlutusType
    , PIsData
    , PEq
    , PHasDatumSchema
    )

instance DerivePlutusType PEnumEncoded where
  type DPTStrat _ = PlutusTypeEnumData

instance PHasExpectedDatumchema PEnumEncoded where
  pexpectedDatumSchema = IntegerSchema

--------------------------------------------------------------------------------
newtype PListEncoded (s :: S)
  = PListEncoded
      ( Term
          s
          ( PDataRecord
              '[ "a" ':= PManyConstructors
               , "b" ':= PValue 'Sorted 'NoGuarantees
               , "c" ':= PCredential
               , "d" ':= PScriptHash
               , "e" ':= PDatumHash
               , "f" ':= PEnumEncoded
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass
    ( PlutusType
    , PIsData
    , PHasDatumSchema
    )

instance DerivePlutusType PListEncoded where
  type DPTStrat _ = PlutusTypeNewtype

instance PHasExpectedDatumchema PListEncoded where
  pexpectedDatumSchema =
    ShapedListSchema $
      NE.fromList $
        wrapDatumSchema
          <$> [ pexpectedDatumSchema @PManyConstructors
              , PlutusSchema ValueSchema
              , PlutusSchema CredentialSchema
              , PlutusSchema Hash28Schema
              , PlutusSchema Hash32Schema
              , pexpectedDatumSchema @PEnumEncoded
              ]
