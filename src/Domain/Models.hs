{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Domain.Models
  ( Task (..),
    TaskId (..),
  )
where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import GHC.Generics
import GHC.Int (Int64)

newtype TaskId = TaskId Int64 deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Task = Task
  { taskId :: TaskId,
    taskName :: Text,
    taskOwner :: Text,
    taskCompleted :: Bool
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''Task)
