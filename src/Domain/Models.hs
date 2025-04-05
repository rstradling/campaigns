{-# LANGUAGE DataKinds #-}
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
import Data.Int (Int64)

newtype TaskId = TaskId Int64 deriving (Eq, Show)

data Task = Task
  { taskId :: Int64,
    taskName :: String,
    taskOwner :: String,
    taskComplete :: Bool
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''Task)
