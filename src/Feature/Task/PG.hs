{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Feature.Task.PG where

import Data.Pool (withResource)
import Database.Beam
import Database.Beam.Postgres
import Feature.Task.Types
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

class (Monad m) => TaskRepo m where
  -- getTask :: Int64 -> m (Maybe Task)
  -- deleteTask :: Int64 -> m (Maybe ())
  getAllTasks :: m [Task]

-- updateTask :: Task -> m (Maybe Task)
-- createTask :: Task -> m (Maybe Task)

instance (HasPgPool env) => TaskRepo (RIO env) where
  {-getTask taskIdentifier = TaskRepoT $ do
    (pool, _) <- ask
    let id64 = getTaskId taskIdentifier
    result <- runBeamPostgres con $ runSelectReturningList $ select $ do
      task <- all_ (_campaignTasks campaignsDb)
      guard_
        (_dbTaskId task ==. val_ id64)
        return
        task
      return $ case result of
        (dbTask : _) -> Just $ dbTaskToDomainTask dbTask
        [] -> Nothing
    return result
    -}

  getAllTasks = do
    poolCon <- view pgPoolL
    dbTasks <- liftIO $ withResource poolCon $ \conn ->
      runBeamPostgres conn $ runSelectReturningList $ select $ all_ (_campaignTasks campaignsDb)
    pure $ dbTaskToDomainTask <$> dbTasks

dbTaskToDomainTask :: DbTask -> Task
dbTaskToDomainTask dbTask =
  Task
    { taskId = TaskId (_dbTaskId dbTask),
      taskName = _dbTaskName dbTask,
      taskOwner = _dbTaskOwner dbTask,
      taskCompleted = _dbTaskCompleted dbTask
    }
