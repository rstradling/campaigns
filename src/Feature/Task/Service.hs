{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Feature.Task.Service where

import qualified Feature.Task.Repo as TaskRepo
import Feature.Task.Types
import RIO

-- * Task

class TaskService m where
  getTask :: TaskId -> m (Maybe Task)
  deleteTask :: TaskId -> m (Maybe ())
  getAll :: m [Task]
  update :: Task -> m (Maybe Task)
  create :: Task -> m (Maybe Task)

instance TaskService (RIO AppEnv) where
  getTask i = TaskRepo.getTask i

  deleteTask i = TaskRepo.deleteTask i
  getAll = TaskRepo.getAllTasks

  update t = TaskRepo.updateTask t
  create t = TaskRepo.createTask t
