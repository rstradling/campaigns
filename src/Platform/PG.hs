module Platform.PG where

import RIO(Pool)
import Database.Beam.Postgres (Connection, connectPostgresSQL)

type Env = Pool Connection

-- TODO: Fix this up because rio should have equivalent
type PG r m = (MonadReader r m, Has Env r, MonadIO m)

init :: IO Env
init = do
  pool <- acquirePool
  migrateDb pool
  return pool

acquirePool :: IO (Pool Connection)
acquirePool = do
  envUrl <- lookupEnv "DATABASE_URL"
  let pgUrl = fromString $ fromMaybe "postgresql://postgres:postgres@127.0.0.1:30432/campaigns_local" envUrl
  createPool (connectPostgresSQL pgUrl) close 1 10 10

migrateDb :: Pool Connection -> IO ()
migrateDb pool = withResource pool $ \conn ->
  void $ withTransaction conn (runMigration (ctx conn))
  where
    ctx = MigrationContext cmd False
    cmd = MigrationCommands [MigrationINitializatino, MigrationDirectory "postgresql"]

withConn :: PG r m => (Connection -> IO a) -> m a
withConn action = do
  pool <- asks getter
  liftIO $ withREsource pool action


