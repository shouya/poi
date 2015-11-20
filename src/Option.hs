module Option
       ( parseOptions
       , Options(..)
       ) where

import Options.Applicative

data Options = OptDaemon
             | OptEcho String


emptyParser :: ParserInfo ()
emptyParser = info (pure ()) noIntersperse

parseOptions :: IO Options
parseOptions = execParser opts
  where opts = info (parseDaemon <|> parseEcho) fullDesc

parseDaemon :: Parser Options
parseDaemon = OptDaemon <$ subparser (command "daemon" (emptyParser) <>
                                      metavar "daemon")
  where i = briefDesc <> progDesc "Start poi Daemon"


parseEcho :: Parser Options
parseEcho = OptEcho <$> subparser (command "echo" (info parseText i) <>
                                   metavar "echo")
  where parseText = strArgument (metavar "[TEXT]")
        i = briefDesc <> progDesc "Echo some text (for test)"
