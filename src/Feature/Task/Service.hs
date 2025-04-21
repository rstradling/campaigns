{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Feature.Task.Service where

import qualified Feature.Task.PG as TaskRepository
import Feature.Task.Types
import RIO

-- * Task

class (Monad m) => TaskService m where
  -- getTask :: Int64 -> m (Maybe Task)
  -- deleteTask :: Int64 -> m (Maybe ())
  getAll :: m [Task]

-- update :: Task -> m (Maybe Task)
-- create :: Task -> m (Maybe Task)

instance (Monad m, TaskRepository.TaskRepo m) => TaskService m where
  -- getTask i = TaskServiceT $ TaskRepository.runTaskRepoT $ TaskRepository.getTask i
  -- deleteTask i = TaskServiceT $ TaskRepository.runTaskRepoT $ TaskRepository.deleteTask i
  getAll = TaskRepository.getAllTasks

-- update t = TaskServiceT $ TaskRepository.runTaskRepoT $ TaskRepository.updateTask t
-- create t = TaskServiceT $ TaskRepository.runTaskRepoT $ TaskRepository.createTask t
