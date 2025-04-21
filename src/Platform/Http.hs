module Platform.Http (main) where

import Conferer
import Control.Lens.TH (makeLenses)
import Data.Pool (Pool)
import Database.Beam.Postgres
import qualified Feature.Task.Http as Task
import Feature.Task.PG (HasPgConn, TaskRepoT (runTaskRepoT))
import Feature.Task.Service (TaskService)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Platform.App
import Platform.Config
import qualified Platform.PG as PG
import RIO
import RIO.Lens (Lens', view)
import Servant (Proxy (..), hoistServer, serve)

app :: AppEnv -> Application
app env = logStdoutDev . simpleCors $ serve (Proxy :: Proxy Task.TaskAPI) (hoistServer (Proxy :: Proxy Task.TaskAPI) (runRIO env) server)

runServer :: AppConfig -> IO ()
runServer config = do
  let port = appConfigPort config
  let conStr = appPgSqlUrl config
  pool <- PG.init conStr
  let env = AppEnv pool
  run 8080 (app env)
