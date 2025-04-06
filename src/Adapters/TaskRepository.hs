module Adapters.TaskRepository
  ( createRepoInstance,
    getAll,
  )
where

import Database.Beam.Postgres (Connection)
import Domain.Models as DModels
import Domain.Ports as Ports

repoSave :: Connection -> DModels.Task -> IO ()
repoSave _ _ =
  return ()

repoGet :: Connection -> DModels.TaskId -> IO DModels.Task
repoGet _ _ =
  return
    DModels.Task
      { taskId = DModels.TaskId 32,
        taskName = "foo",
        taskOwner = "bar",
        taskCompleted = False
      }

repoDelete :: Connection -> DModels.TaskId -> IO ()
repoDelete _ _ =
  return ()

repoGetAll :: Connection -> () -> IO [DModels.Task]
repoGetAll _ _ =
  return []

createRepoInstance :: Connection -> IO Ports.TaskRepository
createRepoInstance conn = do
  return
    Ports.TaskRepository
      { save = repoSave conn,
        get = repoGet conn,
        delete = repoDelete conn,
        getAll = repoGetAll conn
      }
