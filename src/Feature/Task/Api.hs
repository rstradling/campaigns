{-# LANGUAGE TypeOperators #-}

module Feature.Task.Api
  ( API,
    server,
  )
where

import Feature.Task.Service
import Feature.Task.Types
import RIO hiding (Handler)
import Servant (Get, JSON, ServerT, (:>))

type API = "api" :> "v1" :> "tasks" :> Get '[JSON] [Task]

server :: ServerT API (RIO AppEnv)
server = getAll

--  get "/api/v1/tasks/:id" $ do
--    identifier <- pathParam "id"
--    task <- lift $ getTask identifier
--    viewTask task
