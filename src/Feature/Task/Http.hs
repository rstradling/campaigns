{-# LANGUAGE TypeOperators #-}

module Feature.Task.Http
  ( TaskAPI,
    server,
  )
where

import Feature.Task.Service
import Feature.Task.Types
import Servant (Get, Handler, JSON, Server, (:>))

type TaskAPI = "api" :> "v1" :> "tasks" :> Get '[JSON] [Task]

server :: (TaskService Handler) => Server TaskAPI
server = getAll

--  get "/api/v1/tasks/:id" $ do
--    identifier <- pathParam "id"
--    task <- lift $ getTask identifier
--    viewTask task

-- viewTask :: (MonadUnliftIO m) => Maybe Task -> ActionT Text m ()
-- viewTask Nothing = do
--  status status404
--  json $ object ["error" .= ("Task not found" :: String)]
-- viewTask (Just task) = json task

{-taskErrorHandler :: (ScottyError e, Monad m) => TaskError -> ActionT e m ()
taskErrorHandler = case err of
  TaskErrorNotFound _ -> do
    status status404
    json err
-}
