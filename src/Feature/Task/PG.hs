{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Feature.Task.PG where

import Database.Beam
import Database.Beam.Postgres
import Domain.Models as DModels
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

repoGet :: Connection -> DModels.TaskId -> IO (Maybe DModels.Task)
repoGet con taskIdentifier =
  let id64 = getTaskIdValue taskIdentifier
   in do
        result <- runBeamPostgres con $ runSelectReturningList $ select $ do
          task <- all_ (_campaignTasks campaignsDb)
          guard_ (_dbTaskId task ==. val_ id64)
          return task
        return $ case result of
          (dbTask : _) -> Just $ dbTaskToDomainTask dbTask
          [] -> Nothing

repoDelete :: Connection -> Proxy Task -> DModels.TaskId -> IO (Maybe ())
repoDelete _ _ _ =
  return Nothing

repoUpdate :: Connection -> DModels.Task -> IO ()
repoUpdate _ _ =
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
