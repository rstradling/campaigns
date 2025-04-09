module Lib
  ( routes,
    pgConnect,
  )
where

import Adapters.TaskRepository (CrudRepository (..))
import Data.Aeson (KeyValue ((.=)), object)
import Database.Beam
import Database.Beam.Postgres (Connection, connectPostgreSQL)
import Domain.Models.Feature.Task (Task, createTaskId)
import qualified Feature.Task.Http as TaskHttp
import qualified Feature.Task.PG as TaskPG
import qualified Feature.Task.Service as TaskService
import Network.HTTP.Types.Status (status404)
import RIO
import Web.Scotty

type Env = PG.Env

main :: IO ()
main = do
  -- acquire resources
  pgEnv <- PG.init
  -- start the app
  let runner app = flip runReaderT pgEnv $ unAppT app
  Http.main runner

type Env = PG.Env

newtype AppT a = AppT
  { unAppT :: ReaderT Env IO a
  }
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadIO,
      MonadReader Env
    )

instance TaskHttp.TaskService AppT where
  getUser = TaskService.getUser
  deleteUser = TaskService.deleteUser
  getAll = TaskService.getAll
  update = TaskService.update
  create = TaskService.create

instance TaskService.TaskRepo AppT where
  getUser = TaskPG.getUser
  deleteUser = TaskPG.deleteUser
  getAll = TaskPG.getAll
  update = TaskPG.update
  create = TaskPG.create
