module Platform.Config where

import qualified Conferer
import qualified Conferer.Source.CLIArgs as Cli
import qualified Conferer.Source.Env as Env
import Conferer.Source.PropertiesFile as PropFile
import RIO

data AppConfig = AppConfig
  { appConfigPort :: Int,
    appPgSqlUrl :: Text
  }
  deriving (Eq, Show, Generic)

instance Conferer.FromConfig AppConfig

instance Conferer.DefaultConfig AppConfig where
  configDef =
    AppConfig
      { appConfigPort = 3100,
        appPgSqlUrl = "postgresql://postgres:postgres@127.0.0.1:30432/campaigns_local"
      }

mkMyConfig :: IO Conferer.Config
mkMyConfig =
  Conferer.mkConfig'
    []
    [ Cli.fromConfig,
      Env.fromConfig "campaigns",
      PropFile.fromConfig "dev"
    ]
