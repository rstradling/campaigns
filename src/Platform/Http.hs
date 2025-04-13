module Platform.Http (main) where

import Conferer
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import Platform.App
import Platform.Config
import RIO
import Web.Scotty
import qualified Feature.Task.Http as Task

runServer :: (MonadReader MyApp m, MonadUnliftIO m) => m ()
runServer = do
  env <- ask
  let MyApp c _ = env
  liftIO $ scotty (appConfigPort c) $ do
    middleware logStdoutDev
    middleware simpleCors
    get "/search/" $ do
      json (["hello"] :: [String])
-- feature routes
    Task.routes

main :: IO ()
main = do
  logOptions <- logOptionsHandle stdout True
  --  processContext <- mkDefaultProcessContext
  config <- mkMyConfig
  appConfig :: AppConfig <- Conferer.fetch config
  withLogFunc logOptions $ \logFunc ->
    runRIO (MyApp appConfig logFunc) runServer
