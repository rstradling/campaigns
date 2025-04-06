{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( startApp,
    app,
  )
where

import Adapters.TaskRepository (createRepoInstance, taskRepositoryGetAll)
import Database.Beam
import Database.Beam.Postgres (connectPostgreSQL)
import Domain.Models (Task)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Prelude

type API = "tasks" :> Get '[JSON] [Task]

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = do
  conn <- liftIO $ connectPostgreSQL "postgresql://postgres:postgres@127.0.0.1:30432/campaigns_local"
  repo <- liftIO $ createRepoInstance conn
  liftIO $ taskRepositoryGetAll repo
