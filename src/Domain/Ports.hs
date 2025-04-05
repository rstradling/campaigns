module Domain.Ports
  ( TaskRepository (..),
  )
where

import Domain.Models as Domain

data TaskRepository = TaskRepository
  { save :: Domain.Task -> IO (),
    get :: Domain.TaskId -> IO Domain.Task,
    delete :: Domain.TaskId -> IO (),
    getAll :: () -> IO [Domain.Task]
  }
