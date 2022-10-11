module AgoraRegistry.Parsing (
  parseHash,
  parseHex,
) where

import Control.Monad (guard)
import Data.Aeson.Types qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16 (decodeBase16)
import Data.ByteString.UTF8 qualified as UTF8 (fromString)
import Data.Text qualified as Text
import PlutusLedgerApi.V2 qualified as Plutus

parseHex :: String -> Aeson.Parser ByteString
parseHex s = either (fail . Text.unpack) pure (Base16.decodeBase16 (UTF8.fromString s))

parseHash :: Int -> String -> Aeson.Parser Plutus.BuiltinByteString
parseHash l s = do
  guard $ length s == (2 * l)
  bts <- either (fail . Text.unpack) pure (Base16.decodeBase16 (UTF8.fromString s))
  pure $ Plutus.toBuiltin bts
