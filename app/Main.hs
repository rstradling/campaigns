{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Adapters.TaskRepository (createRepoInstance)
import Data.ByteString.Char8 (pack)
import Lib

main :: IO ()
main = do
  conn <- pgConnect (pack "postgresql://postgres:postgres@127.0.0.1:30432/campaigns_local")
  repo <- createRepoInstance conn
  routes repo
