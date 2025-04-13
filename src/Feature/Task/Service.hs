module Feature.Task.Service where

import Feature.Task.Types
import RIO

-- * Task

class (MonadUnliftIO m) => TaskRepo m where
  getTask :: Int64 -> m (Maybe Task)
  deleteTask :: Int64 -> m (Maybe ())
  getAll :: m [Task]
  update :: Task -> m (Maybe Task)
  create :: Task -> m (Maybe Task)

getATask :: (Monad m) => Int64 -> m (Maybe Task)
getATask _ =
  return Nothing

deleteATask :: (Monad m) => Int64 -> m (Maybe ())
deleteATask _ =
  return Nothing

getAnAll :: (Monad m) => m [Task]
getAnAll =
  return
    []

updateTask :: (Monad m) => Task -> m (Maybe Task)
updateTask _ =
  return Nothing

createTask :: (Monad m) => Task -> m (Maybe Task)
createTask _ =
  return Nothing
