module Domain.Ports
  ( TaskRepository (..),
  )
where

import Domain.Models as Domain
import RIO (IO, Int64, Maybe)

data TaskRepository = TaskRepository
  { taskRepositorySave :: Domain.Task -> IO (),
    taskRepositoryGet :: Int64 -> IO (Maybe Domain.Task),
    taskRepositoryDelete :: Domain.TaskId -> IO (),
    taskRepositoryGetAll :: IO [Domain.Task]
  }
