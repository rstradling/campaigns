module Lib
  ( main,
  )
where

import qualified Feature.Task.PG as TaskPG
import qualified Feature.Task.Service as TaskService
import Platform.Config
import qualified Platform.Http as Http
import RIO

main :: IO ()
main = do
  config <- mkMyConfig
  appConfig :: AppConfig <- Conferer.fetch config
  runServer config
