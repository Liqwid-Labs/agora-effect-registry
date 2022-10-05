module AgoraRegistry.Schema (
  validate,
  Effect (..),
  Schema (..),
  Metadata (..),
  plutusValidates,
  runTests
) where

import Control.Applicative (Const(Const))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.Functor.Identity (Identity (Identity))
import Data.Text (Text)
import PlutusLedgerApi.V2 qualified as Plutus
import Data.List.Extra ( (!?) )
import Data.Maybe (fromMaybe)
import GHC.Num (integerToInt)

data Effect f = Effect
  { metadata :: Metadata
  , scriptHash :: Plutus.ScriptHash
  , schema :: Schema f
  }

data Schema f
  = ListSchema (f ()) [Schema f]
  | ConstrSchema (f ()) Integer [Schema f]
  | IntegerSchema (f Integer)
  | ByteStringSchema (f ByteString)
  | OneOfSchema (f ()) [Schema f]
  | PlutusSchema (f Aeson.Value) Text


runTests :: [Bool]
runTests = map (\(s,v,b) -> b == validate s v) tests
 where
  tests =
    [ (constrSchema, constrValue1, True)
    , (constrSchema, constrValue2,False)
    , (listSchema, listValue1, False)
    , (listSchema, listValue2, True)
    , (listSchema, listValue3, False)
    ]
  constrSchema = ConstrSchema (Const ()) 0 [ByteStringSchema (Const ()), IntegerSchema (Const ())]
  constrValue1 = ConstrSchema (Identity ()) 1 [IntegerSchema (Identity 1)]
  constrValue2 = ConstrSchema (Identity ()) 1 [ByteStringSchema (Identity "wrong")]
  listSchema = ListSchema (Const ()) [IntegerSchema (Const ())]
  listValue1 = ListSchema (Identity ()) [ByteStringSchema (Identity "asd"), IntegerSchema (Identity 1)]
  listValue2 = ListSchema (Identity ()) [IntegerSchema (Identity 2), IntegerSchema (Identity 1)]
  listValue3 = ListSchema (Identity ()) [ByteStringSchema (Identity "foo"), ByteStringSchema (Identity "asd")]

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = Just x


validate :: Schema (Const a) -> Schema Identity -> Bool
validate (ListSchema _ children) (ListSchema _ children') = fromMaybe False $ do
  schema <- headMay children
  pure $ all (validate schema) children'

validate (ConstrSchema _ _ fields) (ConstrSchema _ tag' fields') = fromMaybe False $ do
  schema <- fields !? integerToInt tag'
  value <- headMay fields'
  pure $ validate schema value
validate (ByteStringSchema _) (ByteStringSchema (Identity _bs)) = True
validate (IntegerSchema _) (IntegerSchema (Identity _bs)) = True
validate (OneOfSchema _ schemas) (OneOfSchema _ val) = fromMaybe False $ do
  value <- headMay val
  pure $ all (`validate` value) schemas
validate (PlutusSchema _ template) (PlutusSchema (Identity value) template') =
  template == template'
    && plutusValidates template value
validate _ _ = False

plutusValidates :: Text -> Aeson.Value -> Bool
plutusValidates = undefined

data Metadata = Metadata
  { name :: Text
  , description :: Text
  }
