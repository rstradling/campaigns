module Domain.Ports
  ( TaskRepository (..),
  )
where

import Domain.Models as Domain

data TaskRepository = TaskRepository
  { taskRepositorySave :: Domain.Task -> IO (),
    taskRepositoryGet :: Domain.TaskId -> IO Domain.Task,
    taskRepositoryDelete :: Domain.TaskId -> IO (),
    taskRepositoryGetAll :: IO [Domain.Task]
  }
