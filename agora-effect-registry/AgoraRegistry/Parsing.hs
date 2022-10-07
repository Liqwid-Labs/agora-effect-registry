module AgoraRegistry.Parsing (
  parseHash
) where

import Control.Monad (guard)
import Data.Aeson.Types qualified as Aeson
import qualified Data.Text as Text
import qualified Data.ByteString.UTF8 as UTF8 (fromString)
import qualified Data.ByteString.Base16 as Base16 (decodeBase16)
import PlutusLedgerApi.V2 qualified as Plutus


parseHash :: Int -> String -> Aeson.Parser Plutus.ScriptHash
parseHash l s = Plutus.ScriptHash <$> do
  guard (length s == (2*l))
  bts <- either (fail . Text.unpack) pure (Base16.decodeBase16 (UTF8.fromString s))
  pure $ Plutus.toBuiltin bts
