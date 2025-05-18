module Platform.Config where

import qualified Conferer
-- import qualified Conferer.Source.CLIArgs as Cli
-- import qualified Conferer.Source.Env as Env
import Conferer.Source.PropertiesFile as PropFile
import RIO

data AppConfig = AppConfig
  { webPort :: Int,
    dbUrl :: Text
  }
  deriving (Eq, Show, Generic)

instance Conferer.FromConfig AppConfig

instance Conferer.DefaultConfig AppConfig where
  configDef =
    AppConfig
      { webPort = 3100,
        dbUrl = "postgresql://postgres:postgres@127.0.0.1:5432/campaigns_local"
      }

mkMyConfig :: IO AppConfig
mkMyConfig = do
  config <-
    Conferer.mkConfig'
      []
      [ -- Cli.fromConfig,
        -- Env.fromConfig "campaigns",
        PropFile.fromFilePath "config.properties"
      ]
  Conferer.fetch config
