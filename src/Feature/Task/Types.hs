{-# LANGUAGE DeriveAnyClass #-}

module Feature.Task.Types where

import Data.Aeson
import RIO
import qualified RIO.Text as Text
import Servant (FromHttpApiData (..), ToHttpApiData (..))

newtype TaskId = TaskId Int64 deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance FromHttpApiData TaskId where
  parseUrlPiece txt = TaskId <$> parseUrlPiece txt
  parseQueryParam txt = TaskId <$> parseQueryParam txt

instance ToHttpApiData TaskId where
  toUrlPiece (TaskId i) = Text.pack (show i)

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

data TaskHttpError = NotFound TaskId deriving (Typeable)

instance Show TaskHttpError where
  show (NotFound taskId') = "Could not find the task with id: " ++ show taskId'

instance Exception TaskHttpError

data ErrorResponse = ErrorResponse
  { errorMessage :: Text
  }
  deriving (Generic)

instance ToJSON ErrorResponse where
  toJSON (ErrorResponse msg) = object ["error" .= msg]
