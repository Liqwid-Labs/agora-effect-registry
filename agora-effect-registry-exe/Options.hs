module Options (Commands (..), parseOptions) where

import AgoraRegistry.Server.Options (
  HttpServerOptions (HttpServerOptions),
 )
import Control.Applicative (Alternative (some))
import Options.Applicative ((<**>))
import Options.Applicative qualified as Opt

data Commands
  = Verify [FilePath]
  | Serve FilePath HttpServerOptions

parseOptions :: IO Commands
parseOptions = Opt.execParser p
  where
    p =
      Opt.info
        (commands <**> Opt.helper)
        ( Opt.fullDesc
            <> Opt.progDesc "The Agora Effect Registry service."
        )

commands :: Opt.Parser Commands
commands =
  Opt.subparser
    ( Opt.command
        "verify"
        ( Opt.info
            verifyCommandOpts
            (Opt.progDesc "Verify effetc schemas")
        )
        <> Opt.command
          "serve"
          ( Opt.info
              serveCommandOpts
              (Opt.progDesc "Serve the effect registry")
          )
    )

verifyCommandOpts :: Opt.Parser Commands
verifyCommandOpts =
  Verify
    <$> some
      ( Opt.argument
          Opt.str
          ( Opt.metavar "FILES..."
              <> Opt.help "Paths of effect schemas to be validated."
          )
      )

serverOpts :: Opt.Parser HttpServerOptions
serverOpts =
  HttpServerOptions
    <$> Opt.option
      Opt.auto
      ( Opt.long "port"
          <> Opt.short 'p'
          <> Opt.metavar "PORT"
          <> Opt.value 3838
          <> Opt.help "The port to run the registry server on."
      )
    <*> Opt.switch
      ( Opt.long "enable-cors-middleware"
          <> Opt.short 'c'
          <> Opt.help
            ( unwords
                [ "Enable CORS middleware."
                , "This is usually required for some local servers."
                , "For security reasons, this should be disabled in production."
                ]
            )
      )

serveCommandOpts :: Opt.Parser Commands
serveCommandOpts =
  flip Serve
    <$> serverOpts
    <*> Opt.argument
      Opt.str
      ( Opt.metavar "DIR"
          <> Opt.value "./effects"
          <> Opt.help "Path to the directory that contains effect schemas."
      )
