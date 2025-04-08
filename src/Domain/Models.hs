{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

module Domain.Models
  ( Task (..),
    TaskId (..),
    getTaskIdValue,
  )
where

import Data.Aeson
import GHC.Generics
import RIO (Bool, Eq, Int64, Show, Text)

newtype TaskId = TaskId Int64 deriving (Eq, Show, Generic, ToJSON, FromJSON)

getTaskIdValue :: TaskId -> Int64
getTaskIdValue (TaskId i) = i

data Task = Task
  { taskId :: TaskId,
    taskName :: Text,
    taskOwner :: Text,
    taskCompleted :: Bool
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
