module Platform.Http
( main ) where

import Rio
import Network.Wai (Response)

import qualified Feature.Task.Http as Task

type App r m = (User.Service m, MonadIO m)

main :: (App r m) => (m Response -> IO Response) -> IO ()
main runner = do
  port <- acquirePort
  scottyT port runner routes
  where
    acquirePort = do
      port <- fromMaybe "" <$> lookupEnv "PORT"
      return . fromMaybe 3000 $ readMay port

-- * Routing
routes :: (App r m) => ScottyT LazyText m ()
routes = do
  defaultHandler $ \str -> do
    status status500
    json str

  -- feature routes
  Task.routes

  -- health
  get "/api/health" $
    json true
