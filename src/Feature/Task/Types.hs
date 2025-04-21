{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Feature.Task.Types where

import Data.Aeson
import Data.Pool
import Database.Beam.Postgres (Connection)
import RIO

newtype TaskId = TaskId Int64 deriving (Eq, Show, Generic, ToJSON, FromJSON)

getTaskId :: TaskId -> Int64
getTaskId (TaskId i) = i

createTaskId :: Int64 -> TaskId
createTaskId = TaskId

data Task = Task
  { taskId :: TaskId,
    taskName :: Text,
    taskOwner :: Text,
    taskCompleted :: Bool
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data TaskError = TaskErrorNotFound TaskId deriving (Eq, Show, Generic, ToJSON, FromJSON)

data AppEnv = AppEnv
  { _pgPool :: Pool Connection
  }

type AppM = RIO AppEnv

class HasPgConn env where
  pgPoolL :: Lens' env (Pool Connection)

instance HasPgConn AppEnv where
  pgPoolL = lens _pgPool (\x v -> x {_pgPool = v})
