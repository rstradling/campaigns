{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Adapters.TaskRepository
  ( createRepoInstance,
    taskRepositoryGetAll,
    taskRepositoryGet,
  )
where

import Database.Beam
import Database.Beam.Postgres
import Domain.Models as DModels
import Domain.Ports as Ports
import RIO

data DbTaskT f
  = DbTaskT
  { _dbTaskId :: Columnar f Int64,
    _dbTaskName :: Columnar f Text,
    _dbTaskOwner :: Columnar f Text,
    _dbTaskCompleted :: Columnar f Bool
  }
  deriving (Generic)

instance Beamable DbTaskT

instance Table DbTaskT where
  data PrimaryKey DbTaskT f = DbTaskId (Columnar f Int64)
    deriving (Generic, Beamable)
  primaryKey = DbTaskId . _dbTaskId

type DbTask = DbTaskT Identity

deriving instance Show DbTask

deriving instance Eq DbTask

data CampaignsDb f = CampaignsDb
  {_campaignTasks :: f (TableEntity DbTaskT)}
  deriving (Generic)

deriving instance Database be CampaignsDb

campaignsDb :: DatabaseSettings be CampaignsDb
campaignsDb = defaultDbSettings

repoSave :: Connection -> DModels.Task -> IO ()
repoSave _ _ =
  return ()

repoGet :: Connection -> Int64 -> IO (Maybe DModels.Task)
repoGet con taskIdentifier = do
  result <- runBeamPostgres con $ runSelectReturningList $ select $ do
    task <- all_ (_campaignTasks campaignsDb)
    guard_ (_dbTaskId task ==. val_ taskIdentifier)
    return task
  return $ case result of
    (dbTask : _) -> Just $ dbTaskToDomainTask dbTask
    [] -> Nothing

repoDelete :: Connection -> DModels.TaskId -> IO ()
repoDelete _ _ =
  return ()

repoGetAll :: Connection -> IO [DModels.Task]
repoGetAll conn = do
  dbTasks <- runBeamPostgres conn $ runSelectReturningList $ select $ all_ (_campaignTasks campaignsDb)
  return $ map dbTaskToDomainTask dbTasks

dbTaskToDomainTask :: DbTask -> DModels.Task
dbTaskToDomainTask dbTask =
  DModels.Task
    { taskId = DModels.TaskId (_dbTaskId dbTask),
      taskName = _dbTaskName dbTask,
      taskOwner = _dbTaskOwner dbTask,
      taskCompleted = _dbTaskCompleted dbTask
    }

createRepoInstance :: Connection -> IO Ports.TaskRepository
createRepoInstance conn = do
  return
    Ports.TaskRepository
      { taskRepositorySave = repoSave conn,
        taskRepositoryGet = repoGet conn,
        taskRepositoryDelete = repoDelete conn,
        taskRepositoryGetAll = repoGetAll conn
      }
