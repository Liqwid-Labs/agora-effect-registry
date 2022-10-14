module Main (main) where

import AgoraRegistry.DatumValidation (validateEffectDatum)
import AgoraRegistry.Parsing (parseHex')
import AgoraRegistry.Schema (EffectSchema)
import Codec.Serialise (serialise)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseEither, (.=))
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Base16 (encodeBase16)
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Either.Extra (mapLeft)
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy (Proxy (Proxy))
import Network.Wai.Handler.Warp (run)
import Optics.Core (view)
import qualified PlutusLedgerApi.V2 as Plutus
import Servant (
  Capture,
  FromHttpApiData (parseUrlPiece),
  Handler,
  HasServer (ServerT),
  JSON,
  Post,
  Raw,
  ReqBody,
  ServerError (errBody),
  err400,
  err404,
  hoistServer,
  serveDirectoryFileServer,
  throwError,
  (:<|>) ((:<|>)),
  type (:<|>),
  type (:>),
 )
import Servant.Server (Application, serve)
import System.Directory.Extra (listDirectory)

type API =
  "effects" :> Raw
    :<|> ( "encodeEffectDatum"
            :> Capture "scriptHash" EffectScriptHash
            :> ReqBody '[JSON] Aeson.Value
            :> Post '[JSON] EffectDatum
         )

instance Aeson.ToJSON EffectDatum where
  toJSON (EffectDatum ed) =
    Aeson.object ["cborDatum" .= encodeBase16 (serialise ed)]

newtype EffectScriptHash = EffectScriptHash ByteString
  deriving stock (Eq)

instance FromHttpApiData EffectScriptHash where
  parseUrlPiece t =
    EffectScriptHash
      <$> mapLeft (const "Invalid effect script hash.") (parseEither (parseHex' 28) t)

newtype EffectRegistry = EffectRegistry (Map ByteString EffectSchema)

newtype EffectDatum = EffectDatum Plutus.Data

lookupEffectByScriptHash ::
  EffectScriptHash ->
  EffectRegistry ->
  Maybe EffectSchema
lookupEffectByScriptHash (EffectScriptHash k) (EffectRegistry m) = Map.lookup k m

newtype AppM a = AppM (EffectRegistry -> Handler a)

runApp :: EffectRegistry -> AppM a -> Handler a
runApp r (AppM fh) = fh r

askEffect :: EffectScriptHash -> AppM EffectSchema
askEffect hash = AppM $ \r -> case lookupEffectByScriptHash hash r of
  Nothing ->
    throwError $
      err404 {errBody = "Effect with given script is not registered."}
  Just eff -> pure eff

encodeDatum :: EffectScriptHash -> Aeson.Value -> AppM EffectDatum
encodeDatum hash jsonDatum = AppM $
  \r -> case lookupEffectByScriptHash hash r of
    Nothing ->
      throwError $
        err404 {errBody = "Effect with given script is not registered."}
    Just _ -> do
      effSchema <- runApp r $ askEffect hash
      let dataE = validateEffectDatum effSchema jsonDatum
      case dataE of
        Left x ->
          throwError $
            err400
              { errBody =
                  "Provided datum failed to encode against the effect schema. "
                    <> fromString x
              }
        Right d -> pure $ EffectDatum d

api :: Data.Proxy.Proxy API
api = Data.Proxy.Proxy

effectsDir :: FilePath
effectsDir = "./effects"

loadEffects :: IO EffectRegistry
loadEffects = do
  effFiles <- listDirectory effectsDir <&> fmap (\f -> effectsDir <> "/" <> f)
  effectsE <- traverse loadEffect effFiles
  case sequence effectsE of
    Left (fp, err) ->
      fail $
        concat
          [ "Could not load EffectSchema from file: "
          , fp
          , "\nError: "
          , err
          ]
    Right effs -> pure $ EffectRegistry $ Map.fromList effs

loadEffect ::
  FilePath ->
  IO (Either (FilePath, String) (ByteString, EffectSchema))
loadEffect fp =
  Aeson.eitherDecodeFileStrict' fp
    <&> bimap
      (fp,)
      (\eff -> (view #scriptHash eff, eff))

server :: ServerT API AppM
server =
  serveDirectoryFileServer "./effects/"
    :<|> encodeDatum

app :: EffectRegistry -> Application
app s = serve api $ hoistServer api (runApp s) server

main :: IO ()
main = do
  registry <- loadEffects
  putStrLn "Effect schemas loaded."
  run 9999 (app registry)
