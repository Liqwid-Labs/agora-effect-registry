module AgoraRegistry.Schema (
  validate,
  Effect (..),
  Schema (..),
  Metadata (..),
  plutusValidates,
) where

import Control.Applicative (Const)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.Functor.Identity (Identity (Identity))
import Data.Text (Text)
import PlutusLedgerApi.V2 qualified as Plutus

data Effect f = Effect
  { metadata :: Metadata
  , scriptHash :: Plutus.ScriptHash
  , schema :: Schema f
  }

data Schema f
  = ListSchema (f ()) (Schema f)
  | ConstrSchema (f ()) Integer [Schema f]
  | IntegerSchema (f Integer)
  | ByteStringSchema (f ByteString)
  | OneOfSchema (f (Schema f)) [Schema f]
  | PlutusSchema (f Aeson.Value) Text

validate :: Schema (Const a) -> Schema Identity -> Bool
validate (ListSchema _ children) (ListSchema _ children') = validate children children'
validate (ConstrSchema _ tag fields) (ConstrSchema _ tag' fields') =
  and $ (tag == tag') : zipWith validate fields fields'
validate (ByteStringSchema _) (ByteStringSchema (Identity _bs)) = True
validate (IntegerSchema _) (IntegerSchema (Identity _bs)) = True
validate (OneOfSchema _ options) (OneOfSchema (Identity chosen) _) = any (`validate` chosen) options
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
