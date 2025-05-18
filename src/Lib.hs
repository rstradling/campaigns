module Lib
  ( main,
  )
where

import qualified Platform.AppServer as App
import Platform.Config
import RIO
import System.IO

main :: IO ()
main = do
  config <- mkMyConfig
  _ <- putStrLn $ show config
  App.runServer config
