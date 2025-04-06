{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Adapters.TaskRepository
  ( createRepoInstance,
    taskRepositoryGetAll,
  )
where

import Data.Text (Text)
import Database.Beam
import Database.Beam.Postgres
import Domain.Models as DModels
import Domain.Ports as Ports
import GHC.Int (Int64)

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

type DbTaskId = PrimaryKey DbTaskT Identity

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
