module Option
       ( parseOptions
       , Subcommand(..)
       ) where

import Options.Applicative

data Subcommand = CmdDaemon
                | CmdEcho String
                | CmdBuild
                | CmdSetup String
                | CmdUp
                | CmdInit (Maybe String)


parseOptions :: IO Subcommand
parseOptions = execParser opts
  where opts = info (helper <*> commands) desc
        commands = parseSubcommand CmdDaemon "daemon" "Start poi daemon"   <|>
                   parseSubcommand CmdBuild  "build"  "(Re)build services" <|>
                   parseSubcommand CmdUp     "up"     "(Re)start services" <|>
                   parseSetup                                              <|>
                   parseInit                                               <|>
                   parseEcho
        desc = fullDesc <>
               briefDesc <>
               header "poi: Yet another VPS deployment daemon"

parseSubcommand :: Subcommand -> String -> String -> Parser Subcommand
parseSubcommand subc name desc = subc <$ subparser parser
  where parser = (command name (info emptyParser help) <>
                  metavar name)
        emptyParser = pure ()
        help = briefDesc <> progDesc desc

parseSetup :: Parser Subcommand
parseSetup = CmdSetup <$>
             subparser (command  "setup" (info parseURL i) <>
                        metavar  "setup")
  where parseURL = strArgument (metavar "GIT_REPO")
        i = briefDesc <> progDesc "Clone Git repo and build"

parseInit :: Parser Subcommand
parseInit = CmdInit <$>
            subparser (command "init" (info parseDir i) <>
                       metavar "init")
  where parseDir = optional $ strArgument (metavar "[DIR]")
        i = briefDesc <> progDesc "Generate poi service bundle template"

parseEcho :: Parser Subcommand
parseEcho = CmdEcho <$> subparser (command "echo" (info parseText i) <>
                                   metavar "echo")
  where parseText = strArgument (metavar "TEXT")
        i = briefDesc <> progDesc "Echo some text (for test)"
