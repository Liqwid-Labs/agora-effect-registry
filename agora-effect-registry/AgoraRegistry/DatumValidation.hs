{- |
Module     : AgoraRegistry.DatumValidation
Maintainer : michal@mlabs.city
Description: Encoding and validation of JSON-encoded agora effect datums.

Encoding and validation of JSON-encoded agora effect datums.
-}
module AgoraRegistry.DatumValidation (
  validateEffectDatum,
  validateEffectDatum',
  validateJsonDatum,
) where

import AgoraRegistry.Parsing (
  parseGuard,
  parseHash,
  parseHex,
 )
import AgoraRegistry.Schema (
  DatumSchema (
    ByteStringSchema,
    ConstrSchema,
    IntegerSchema,
    ListSchema,
    MapSchema,
    OneOfSchema,
    PlutusSchema,
    ShapedListSchema
  ),
  EffectSchema,
  PlutusTypeSchema (
    AddressSchema,
    CredentialSchema,
    Hash28Schema,
    Hash32Schema,
    ValueSchema
  ),
  schemaName,
 )
import Control.Applicative ((<|>))
import Control.Monad (when, zipWithM)
import Data.Aeson (withObject, (.:))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.Aeson.Types as Aeson
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.UTF8 (fromString)
import Data.Function ((&))
import Data.List.Extra (groupSortBy)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Ord (comparing)
import qualified Data.Text as T
import Optics.Core (view, (%))
import qualified PlutusLedgerApi.V2 as Plutus

{- | Provided an effect datum schema and a datum encoded in JSON it validates
     the datum conformance to the schema and returns the datum as
     `PlutusLedgerApi.V2.Data` value or a validation error.

     @since 0.1.0
-}
validateEffectDatum :: EffectSchema -> Aeson.Value -> Either String Plutus.Data
validateEffectDatum es = parseEither (validateEffectDatum' es)

{- | Provided an effect datum schema and a datum encoded in JSON it validates
     the datum conformance to the schema and returns the datum as
     `PlutusLedgerApi.V2.Data` value in the format of an `Aeson.Parser`

     @since 0.1.0
-}
validateEffectDatum' :: EffectSchema -> Aeson.Value -> Aeson.Parser Plutus.Data
validateEffectDatum' es = validateJsonDatum (view (#datumSchema % #schema) es)

{- | Provided an effect datum schema and a datum encoded in JSON it validates
     the datum conformance to the schema and returns the datum as
     `PlutusLedgerApi.V2.Data` value in the format of an `Aeson.Parser`

     @since 0.1.0
-}
validateJsonDatum :: DatumSchema -> Aeson.Value -> Aeson.Parser Plutus.Data
validateJsonDatum expectedSchema v = flip (Aeson.withObject "Datum") v $ \o -> do
  jsonSchemaType :: String <- o .: "type"
  addFailMessage jsonSchemaType expectedSchema $
    case (expectedSchema, jsonSchemaType) of
      (OneOfSchema ss, _) ->
        nonEmptyMsum $
          flip validateJsonDatum v . view #schema <$> ss
      (ListSchema s, "list") -> parseList (view #schema s) o
      (ShapedListSchema ss, "shaped_list") ->
        parseShapedList (view #schema <$> ss) o
      (ConstrSchema tag ss, "constr") ->
        parseConstr tag (view #schema <$> ss) o
      (MapSchema ks vs, "map") -> parseMap (view #schema ks) (view #schema vs) o
      (IntegerSchema, "integer") -> Plutus.I <$> (o .: "value")
      (ByteStringSchema, "bytes") -> Plutus.B <$> (parseHex =<< o .: "value")
      (PlutusSchema ps, _) -> parsePlutusType ps v
      _ ->
        fail $
          "Unknown schema type or schema mismatch. Got: "
            <> jsonSchemaType
            <> ", expected: "
            <> show expectedSchema
  where
    -- The default monoid instance uses 'mempty' which ruins error messages.
    nonEmptyMsum (x :| []) = x
    nonEmptyMsum (x :| (x' : xs)) = x <|> nonEmptyMsum (x' :| xs)
    addFailMessage has exp =
      Aeson.modifyFailure
        ( (++) $
            "While validating '" <> has <> "' against '"
              <> schemaName exp
              <> "' schema: "
        )
    -- parses json of shape:
    -- {..., "elements":[[{keyschema},{valueschema}], ... ]}
    parseMap ::
      DatumSchema ->
      DatumSchema ->
      Aeson.Object ->
      Aeson.Parser Plutus.Data
    parseMap ks vs o = do
      elems <-
        o .: "elements"
          >>= traverse (bitraverse (validateJsonDatum ks) (validateJsonDatum vs))
      pure $ Plutus.Map elems

    parseConstr ::
      Integer ->
      NonEmpty DatumSchema ->
      Aeson.Object ->
      Aeson.Parser Plutus.Data
    parseConstr expectedTag ss o = do
      tag <- o .: "tag"
      parseGuard "Constr tag mismatch." (tag == expectedTag)
      fields <- zipWithM validateJsonDatum (NE.toList ss) =<< (o .: "fields")
      pure $ Plutus.Constr tag fields

    parseShapedList ::
      NonEmpty DatumSchema ->
      Aeson.Object ->
      Aeson.Parser Plutus.Data
    parseShapedList ss o =
      Plutus.List
        <$> (zipWithM validateJsonDatum (NE.toList ss) =<< (o .: "elements"))

    parseList :: DatumSchema -> Aeson.Object -> Aeson.Parser Plutus.Data
    parseList elementsSchema o =
      Plutus.List
        <$> (traverse (validateJsonDatum elementsSchema) =<< o .: "elements")

-- | Validates and encodes one the supported plutus type provided as a JSON.
parsePlutusType :: PlutusTypeSchema -> Aeson.Value -> Aeson.Parser Plutus.Data
parsePlutusType s v = flip (Aeson.withObject "PlutusType") v $ \o -> do
  schemaType :: String <- o .: "type"
  case (s, schemaType) of
    (AddressSchema, "plutus/Address") -> Plutus.toData <$> parseAddress v
    (ValueSchema, "plutus/Value") -> Plutus.toData <$> parseValue v
    (CredentialSchema, "plutus/Credential") ->
      Plutus.toData
        <$> parseCredential v
    (Hash32Schema, "plutus/Hash32") ->
      Plutus.toData
        <$> (parseHash 32 =<< o .: "value")
    (Hash28Schema, "plutus/Hash28") ->
      Plutus.toData
        <$> (parseHash 28 =<< o .: "value")
    _ ->
      fail $
        "Type mismatch, expected schema: " <> show s
          <> " got: "
          <> schemaType

{- | Dedicated JSON Parser for Plutus' Credential type.
     Example:
    ```json
    { "type": "plutus/Credential"
    , "script": "aabbccddeeff11223344556677889900aabbccddeeff112233445566"
    }
    ```
-}
parseCredential :: Aeson.Value -> Aeson.Parser Plutus.Credential
parseCredential = addFailMessage ("Parsing credential: " ++) $
  withObject "Credential" $ \o -> do
    ctor <-
      Aeson.modifyFailure (const "Missing 'pubkey' or 'script' field.") $
        Left <$> (o .: "pubkey") <|> Right <$> (o .: "script")
    ctor
      & either
        (fmap (Plutus.PubKeyCredential . Plutus.PubKeyHash) . parseHash 28)
        (fmap (Plutus.ScriptCredential . Plutus.ValidatorHash) . parseHash 28)

{- | Dedicated JSON Parser for Plutus' Address type.

     NOTE: Currently staking credentials are not supported.

     An example of accepted json structure:

    ```json
    { "type": "plutus/Address"
    , "pubkey": "aabbccddeeff11223344556677889900aabbccddeeff112233445566"
    }
    ```
-}
parseAddress :: Aeson.Value -> Aeson.Parser Plutus.Address
parseAddress =
  addFailMessage ("Parsing address: " ++) $
    fmap (`Plutus.Address` Nothing) . parseCredential

{- | Dedicated JSON Parser for Plutus' Value type.
     An example json form that will be accepted:

    ```json
    {
      "type": "plutus/Value",
      "value": [
        { "currencySymbol":"aabbccddeeff11223344556677889900aabbccddeeff112233445566"
        , "tokenName": "sometokenname"
        , "amount":10
        }]
    }
    ```
-}
parseValue :: Aeson.Value -> Aeson.Parser Plutus.Value
parseValue = addFailMessage ("Parsing value: " ++) $
  \v -> flip (Aeson.withObject "plutus/value") v $ \o -> do
    values <- traverse parseFlatten =<< o .: "value"
    valueMap <- either fail pure (buildValueMap values)
    pure $ Plutus.Value valueMap
  where
    buildValueMap ::
      [(Plutus.CurrencySymbol, Plutus.TokenName, Integer)] ->
      Either
        String
        (Plutus.Map Plutus.CurrencySymbol (Plutus.Map Plutus.TokenName Integer))
    buildValueMap = \case
      [] -> pure $ Plutus.fromList []
      xs -> do
        let csGroups = groupSortBy (comparing (\(cs, _, _) -> cs)) xs
        values <- traverse buildInnerMap csGroups
        pure $ Plutus.fromList values

    -- assuming items grouped by currency symbol, non empty groups
    buildInnerMap ::
      [(Plutus.CurrencySymbol, Plutus.TokenName, Integer)] ->
      Either String (Plutus.CurrencySymbol, Plutus.Map Plutus.TokenName Integer)
    buildInnerMap xs = do
      let hasDuplicates =
            any ((<) 1 . length) $
              groupSortBy
                (comparing (\(_, tn, _) -> tn))
                xs
      when hasDuplicates $ Left "Duplicated entries found in map"
      let cs = (\(cs, _, _) -> cs) (head xs)
      pure (cs, Plutus.fromList $ (\(_, x, y) -> (x, y)) <$> xs)

    parseFlatten ::
      Aeson.Object ->
      Aeson.Parser (Plutus.CurrencySymbol, Plutus.TokenName, Integer)
    parseFlatten o = do
      csStr <- o .: "currencySymbol"
      cs <-
        Plutus.CurrencySymbol
          <$> if T.null csStr
            then pure (Plutus.toBuiltin ("" :: ByteString))
            else parseHash 28 csStr
      tn' <- fromString <$> (o .: "tokenName")
      let tn = Plutus.TokenName . Plutus.toBuiltin $ tn'
      when (T.null csStr && not (B.null tn')) $
        fail "Currency symbol must be present when token name is present"
      amount <- o .: "amount"
      pure (cs, tn, amount)

-- | Helper function to conveniently modify the error message of a parser.
addFailMessage ::
  (String -> String) ->
  (Aeson.Value -> Aeson.Parser a) ->
  Aeson.Value ->
  Aeson.Parser a
addFailMessage modMessage parse v = Aeson.modifyFailure modMessage (parse v)
