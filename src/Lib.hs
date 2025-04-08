module Lib
  ( routes,
    pgConnect,
  )
where

import Data.Aeson (KeyValue ((.=)), object)
import Database.Beam
import Database.Beam.Postgres (Connection, connectPostgreSQL)
import Domain.Models (Task)
import Domain.Ports (TaskRepository (taskRepositoryGet, taskRepositoryGetAll))
import Network.HTTP.Types.Status (status404)
import RIO
import Web.Scotty

routes :: Domain.Ports.TaskRepository -> IO ()
routes repo = scotty 8080 $ do
  get "/api/v1/tasks/:id" $ do
    taskId <- pathParam "id"
    task <- liftIO $ taskRepositoryGet repo taskId
    viewTask task
  get "/api/v1/tasks/" $ do
    tasks <- liftIO $ taskRepositoryGetAll repo
    tasksList tasks

pgConnect :: ByteString -> IO Connection
pgConnect url = connectPostgreSQL url

tasksList :: [Task] -> ActionM ()
tasksList = json

viewTask :: Maybe Task -> ActionM ()
viewTask Nothing = do
  status status404
  json $ object ["error" .= ("Task not found" :: String)]
viewTask (Just task) = json task
