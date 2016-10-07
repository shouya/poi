module Option
       ( parseOptions
       , Subcommand(..)
       ) where

import Options.Applicative

data Subcommand = CmdDaemon
                | CmdRun
                | CmdGenConf
                | CmdCheckConfig
type ConfigPath = String

parseOptions :: IO (ConfigPath, Subcommand)
parseOptions = execParser opts
  where opts = info (helper <*> parser') desc
        commandParser =
          cmd "daemon"      CmdDaemon      "Start poi daemon"           <|>
          cmd "run"         CmdRun         "Checkout & run script once" <|>
          cmd "genconfig"   CmdGenConf     "Generate default config"    <|>
          cmd "checkconfig" CmdCheckConfig "Check config"
        parser' = (,) <$> configOption <*> commandParser
        desc = fullDesc <>
               briefDesc <>
               header "poi: Portable VPS deployment daemon"

configOption :: Parser ConfigPath
configOption = optionParser <|> pure "vps/poi.conf"
  where optionParser = strOption $
          long "config" <>
          short 'c' <>
          value "vps/poi.conf" <>
          metavar "CONFIG" <>
          help "Specify the location of config file"


cmd :: String -> Subcommand -> String -> Parser Subcommand
cmd name constructor desc =
  subparser $ command name (info (pure constructor) descInfo) <> metavar name
  where descInfo = briefDesc <> progDesc desc
