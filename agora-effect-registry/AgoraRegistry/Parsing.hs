{- |
Module     : AgoraRegistry.Parsing
Maintainer : michal@mlabs.city
Description: Parsing utilities for basic types.

Parsing utilities for basic types.
-}
module AgoraRegistry.Parsing (
  parseHash,
  parseHex,
  parseHex',
  parseGuard,
) where

import Control.Monad (guard)
import qualified Data.Aeson.Types as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16 (decodeBase16)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified PlutusLedgerApi.V2 as Plutus

-- | Like `Control.Monad.guard`, but accepts an error message.
parseGuard :: String -> Bool -> Aeson.Parser ()
parseGuard s = Aeson.modifyFailure (const s) . guard

-- | Decode hex and utf8 encoded bytes
parseHex :: Text -> Aeson.Parser ByteString
parseHex s =
  either
    (fail . Text.unpack)
    pure
    (Base16.decodeBase16 (encodeUtf8 s))

-- | Decode hex and utf8 encoded bytes of particular size (in bytes)
parseHex' :: Int -> Text -> Aeson.Parser ByteString
parseHex' l s = do
  parseGuard
    ( "Required hex string length is: " <> show (2 * l)
        <> " got: "
        <> show s
    )
    $ Text.length s == (2 * l)
  parseHex s

-- | Decode hex and utf8 encoded hashes of particular size (in bytes)
parseHash :: Int -> Text -> Aeson.Parser Plutus.BuiltinByteString
parseHash l s = do
  bts <- parseHex' l s
  pure $ Plutus.toBuiltin bts
