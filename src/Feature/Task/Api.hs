{-# LANGUAGE TypeOperators #-}

module Feature.Task.Api
  ( API,
    server,
  )
where

import Feature.Common.Types (AppEnv)
import Feature.Task.Repo (TaskRepo (getAllTasks))
import Feature.Task.Service
import Feature.Task.Types
import RIO
import Servant

type API =
  "api"
    :> "v1"
    :> "tasks"
    :> Get '[JSON] [Task]
    :<|> "api"
      :> "v1"
      :> "tasks"
      :> Capture "id" TaskId
      :> Get '[JSON] Task

server :: ServerT API (RIO AppEnv)
server = getAllTasks' :<|> getTask'
  where
    getAllTasks' :: RIO AppEnv [Task]
    getAllTasks' = getAllTasks
    getTask' :: TaskId -> RIO AppEnv Task
    getTask' i = do
      maybeTask <- getTask i
      case maybeTask of
        Just task -> pure task
        Nothing -> throwM $ NotFound i

--  get "/api/v1/tasks/:id" $ do
--    identifier <- pathParam "id"
--    task <- lift $ getTask identifier
--    viewTask task
