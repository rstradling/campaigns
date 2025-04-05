module Adapters.TaskRepository
  ( createRepoInstance,
    getAll,
  )
where

import Domain.Models as DModels
import Domain.Ports as Ports

repoSave :: DModels.Task -> IO ()
repoSave _ =
  return ()

repoGet :: DModels.TaskId -> IO DModels.Task
repoGet _ =
  return
    DModels.Task
      { taskId = 32,
        taskName = "foo",
        taskOwner = "bar",
        taskComplete = False
      }

repoDelete :: DModels.TaskId -> IO ()
repoDelete _ =
  return ()

repoGetAll :: () -> IO [DModels.Task]
repoGetAll _ =
  return []

createRepoInstance :: IO Ports.TaskRepository
createRepoInstance = do
  return
    Ports.TaskRepository
      { save = repoSave,
        get = repoGet,
        delete = repoDelete,
        getAll = repoGetAll
      }
