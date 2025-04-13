module Lib
(
main
  )
where

{-import Data.Aeson (KeyValue ((.=)), object)
import Database.Beam
import Database.Beam.Postgres (Connection, connectPostgreSQL)
import qualified Feature.Task.Http as TaskHttp
import qualified Feature.Task.PG as TaskPG
import qualified Feature.Task.Service as TaskService
import Network.HTTP.Types.Status (status404)
import Platform.App
import qualified Platform.Http as Http
import qualified Platform.PG as PG
import Platform.Services
import Web.Scotty
-}
import RIO
import qualified Platform.Http as Http
main :: IO ()
main = do
  Http.main

{-instance TaskService.TaskRepo (RIO MyApp) where
  getTask = TaskPG.getTask
  deleteTask = TaskPG.deleteTask
  getAll = TaskPG.getAll
  update = TaskPG.update
  create = TaskPG.create
  -}
