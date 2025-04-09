module Feature.Task.Service where

import RIO
import Feature.Task.Types

-- * Task


class (Monad m) => TaskRepo m where
  getUser :: Int64 -> m (Maybe DM.Task)
  deleteUser :: Int64 -> m (Maybe ())
  getAll :: m [DM.Task]
  update :: DM.Task -> m (Maybe DM.Task)
  create :: DM.Task -> m (Maybe DM.Task)


