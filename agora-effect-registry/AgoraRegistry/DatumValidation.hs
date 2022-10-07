module AgoraRegistry.DatumValidation (
  validateJsonDatum,
  validateJsonDatum',
) where

import Data.Aeson qualified as Aeson

import PlutusLedgerApi.V2 qualified as Plutus
import Data.Either (isRight)
import AgoraRegistry.Schema (Schema)

validateJsonDatum' :: Schema -> Aeson.Value -> Bool
validateJsonDatum' s = isRight . validateJsonDatum s

validateJsonDatum :: Schema -> Aeson.Value -> Either String Plutus.Data
validateJsonDatum = undefined
