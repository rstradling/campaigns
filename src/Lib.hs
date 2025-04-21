module Lib
  ( main,
  )
where

import Conferer
import Platform.Config
import qualified Platform.Http as Http
import RIO

main :: IO ()
main = do
  config <- mkMyConfig
  appConfig :: AppConfig <- Conferer.fetch config
  Http.runServer appConfig
