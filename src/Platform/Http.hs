module Platform.Http (runServer) where

import qualified Feature.Task.Api as TaskApi
import Feature.Task.Types (AppEnv (..))
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Platform.Config
import qualified Platform.PG as PG
import RIO hiding (Handler)
import Servant (Handler, hoistServer, serve)

app :: AppEnv -> Application
app env =
  logStdoutDev
    . simpleCors
    $ serve
      (Proxy :: Proxy TaskApi.API)
      (hoistServer (Proxy :: Proxy TaskApi.API) (rioToHandler env) TaskApi.server)

rioToHandler :: AppEnv -> RIO AppEnv a -> Handler a
rioToHandler env rio = liftIO (runRIO env rio)

runServer :: AppConfig -> IO ()
runServer config = do
  let port = appConfigPort config
  let conStr = appPgSqlUrl config
  pool <- PG.init conStr
  let env = AppEnv pool
  run port (app env)
