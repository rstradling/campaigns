module Feature.Common.Types where

import Data.Pool
import RIO
import Database.Beam.Postgres(Connection)

data AppEnv = AppEnv
  { _pgPool :: Pool Connection
  }

type AppM = RIO AppEnv

class HasPgPool env where
  pgPoolL :: Lens' env (Pool Connection)

instance HasPgPool AppEnv where
  pgPoolL = lens _pgPool (\x v -> x {_pgPool = v})


