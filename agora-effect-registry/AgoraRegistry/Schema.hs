module AgoraRegistry.Schema (
  validate,
  Effect (..),
  Schema (..),
  Metadata (..),
  runTests,
  treasuryWithdrawalEffectSchema
) where

import Control.Applicative (Const(Const))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.Functor.Identity (Identity (Identity))
import Data.Text (Text)
import PlutusLedgerApi.V2 qualified as Plutus
import Data.List.Extra ( (!?) )
import Data.Maybe (fromMaybe)
import GHC.Num (integerToInt, Natural)
import Data.Either (isRight)
import Data.Default (Default(def))
import Data.Aeson (FromJSON(parseJSON))

data Metadata = Metadata
  { name :: Text
  , description :: Text
  }

instance Aeson.ToJSON Metadata where
  toJSON = undefined

instance Aeson.FromJSON Metadata where
  parseJSON = undefined

instance Default Metadata where def = Metadata "" ""

data Effect = Effect
  { meta :: Metadata
  , scriptHash :: Plutus.ScriptHash
  , datumSchema :: Schema
  }

newtype Constraint = Constraint Aeson.Value -- provisional impl

data Schema = Schema
  { meta :: Metadata
  , constraints :: [Constraint]
  , schema :: DatumSchema
  }

instance Aeson.ToJSON Schema where
  toJSON = undefined

instance Aeson.FromJSON Schema where
  parseJSON = undefined


data DatumSchema
  = ListSchema [Schema]
  | ConstrSchema [[Schema]]
  | OneOfSchema [Schema]
  | MapSchema [(Schema, Schema)]
  | IntegerSchema
  | ByteStringDatum
  | PlutusSchema PlutusTypeSchema
treasuryWithdrawalEffectSchema :: Effect
treasuryWithdrawalEffectSchema = Effect
  { meta = Metadata
    { name = "TreasuryWithdrawal"
    , description = "..."
    }
  , scriptHash =  "..."
  , datumSchema = Schema
    { meta = def { name = "Receivers"}
    , constraints = []
    , schema = ConstrSchema
      [[ Schema def []
        ( ListSchema
          [ Schema def []
            ( ConstrSchema
              [ [ Schema
                 (Metadata {name = "Receiver", description = "The address the value will be sent to" })
                 []
                 (PlutusSchema AddressSchema)]
              , [ Schema def []
                 (PlutusSchema ValueSchema) ]
              ])
          ]
        )
      ]]
    }
  }

instance Aeson.ToJSON Effect where
  toJSON = undefined

instance Aeson.FromJSON Effect where
  parseJSON = undefined


data PlutusTypeSchema
  = AddressSchema
  | ValueSchema
  | CredentialSchema
  | AssetClassSchema
  | Hash32Schema
  | Hash28Schema

instance Aeson.ToJSON PlutusTypeSchema where
  toJSON = undefined

instance Aeson.FromJSON PlutusTypeSchema where
  parseJSON = undefined

validateJsonDatum' :: Schema -> Aeson.Value -> Bool
validateJsonDatum' s = isRight . validateJsonDatum s

validateJsonDatum :: Schema -> Aeson.Value -> Either String Plutus.Data
validateJsonDatum = undefined

------------------------------------

runTests :: [Bool]
runTests = undefined
-- map (\(s,v,b) -> b == validate s v) tests
--  where
--   tests =
--     [ (constrSchema, constrValue1, True)
--     , (constrSchema, constrValue2,False)
--     , (listSchema, listValue1, False)
--     , (listSchema, listValue2, True)
--     , (listSchema, listValue3, False)
--     ]
--   constrSchema = ConstrSchema (Const ()) 0 [ByteStringSchema (Const ()), IntegerSchema (Const ())]
--   constrValue1 = ConstrSchema (Identity ()) 1 [IntegerSchema (Identity 1)]
--   constrValue2 = ConstrSchema (Identity ()) 1 [ByteStringSchema (Identity "wrong")]
--   listSchema = ListSchema (Const ()) [IntegerSchema (Const ())]
--   listValue1 = ListSchema (Identity ()) [ByteStringSchema (Identity "asd"), IntegerSchema (Identity 1)]
--   listValue2 = ListSchema (Identity ()) [IntegerSchema (Identity 2), IntegerSchema (Identity 1)]
--   listValue3 = ListSchema (Identity ()) [ByteStringSchema (Identity "foo"), ByteStringSchema (Identity "asd")]

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = Just x

validate = undefined
-- validate :: Schema (Const a) -> Schema Identity -> Bool
-- validate (ListSchema _ children) (ListSchema _ children') = fromMaybe False $ do
--   schema <- headMay children
--   pure $ all (validate schema) children'

-- validate (ConstrSchema _ _ fields) (ConstrSchema _ tag' fields') = fromMaybe False $ do
--   schema <- fields !? integerToInt tag'
--   value <- headMay fields'
--   pure $ validate schema value
-- validate (ByteStringSchema _) (ByteStringSchema (Identity _bs)) = True
-- validate (IntegerSchema _) (IntegerSchema (Identity _bs)) = True
-- validate (OneOfSchema _ schemas) (OneOfSchema _ val) = fromMaybe False $ do
--   value <- headMay val
--   pure $ all (`validate` value) schemas
-- validate (PlutusSchema _ template) (PlutusSchema (Identity value) template') =
--   template == template'
--     && plutusValidates template value
-- validate _ _ = False

-- plutusValidates :: Text -> Aeson.Value -> Bool
-- plutusValidates = undefined

