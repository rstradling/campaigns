module Lib
  ( main,
  )
where

import Conferer
import qualified Platform.AppServer as App
import Platform.Config
import RIO

main :: IO ()
main = do
  config <- mkMyConfig
  appConfig :: AppConfig <- Conferer.fetch config
  App.runServer appConfig
