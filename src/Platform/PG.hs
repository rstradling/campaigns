module Platform.PG where

import Data.Pool
import Database.Beam.Postgres (Connection, close, connectPostgreSQL)
import RIO

init :: Text -> IO (Pool Connection)
init = acquirePool

--  migrateDb pool

acquirePool :: Text -> IO (Pool Connection)
acquirePool conStr =
  liftIO $ newPool finalConfig
  where
    config = defaultPoolConfig (connectPostgreSQL $ encodeUtf8 conStr) close 60 10
    stripes = Just 2
    finalConfig = setNumStripes stripes $ config

{-migrateDb :: Pool Connection -> IO ()
migrateDb pool = withResource pool $ \conn ->
  void $ withTransaction conn (runMigration (ctx conn))
  where
    ctx = MigrationContext cmd False
    cmd = MigrationCommands [MigrationInitialization, MigrationDirectory "postgresql"]
    -}

{-withConn :: (PG r m) => (Connection -> IO a) -> m a
withConn action = do
  pool <- asks getter
  liftIO $ withREsource pool action
  -}
