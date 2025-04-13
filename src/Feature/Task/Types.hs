{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

module Feature.Task.Types where

import Data.Aeson
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
