{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( startApp,
    app,
  )
where

import Adapters.TaskRepository (createRepoInstance, getAll)
import Control.Monad.IO.Class (liftIO)
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
  repo <- liftIO createRepoInstance
  liftIO $ getAll repo ()
