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

import Adapters.TaskRepository (createRepoInstance, getAll)
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
  conn <- liftIO $ connectPostgreSQL "host=localhost port=30432 dbname=campaigns_local user=postgres password=postgres"
  repo <- liftIO $ createRepoInstance conn
  liftIO $ getAll repo ()
