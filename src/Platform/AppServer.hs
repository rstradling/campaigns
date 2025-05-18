{-# LANGUAGE TypeOperators #-}

module Platform.AppServer (runServer) where

import Data.Aeson (encode)
import qualified Feature.Campaign.Api as CampaignApi
import Feature.Common.Types
import qualified Feature.Task.Api as TaskApi
import Feature.Task.Types (ErrorResponse (..), TaskHttpError (..))
import Network.HTTP.Types (hContentType)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Platform.Config
import qualified Platform.PG as PG
import RIO hiding (Handler)
import Servant (Handler, ServerT, err404, err500, errBody, errHeaders, hoistServer, serve, throwError, (:<|>) (..))

type AppApi = TaskApi.API :<|> CampaignApi.API

app :: AppEnv -> Application
app env =
  logStdoutDev
    . simpleCors
    $ serve
      (Proxy :: Proxy AppApi)
      (hoistServer (Proxy :: Proxy AppApi) (rioToHandler env) appServer)
  where
    appServer :: ServerT AppApi (RIO AppEnv)
    appServer = TaskApi.server :<|> CampaignApi.server

rioToHandler :: AppEnv -> RIO AppEnv a -> Handler a
rioToHandler env rio = do
  result <- liftIO $ try $ runRIO env rio
  case result of
    Right value -> pure value
    Left e -> case fromException e of
      Just err@(NotFound _) ->
        throwError
          $ err404
            { errBody = encode $ ErrorResponse $ tshow err,
              errHeaders = [(hContentType, "application/json")]
            }
      _ ->
        throwError
          $ err500
            { errBody = encode $ ErrorResponse "Unexpected error",
              errHeaders = [(hContentType, "application/json")]
            }

runServer :: AppConfig -> IO ()
runServer config = do
  let port = webPort config
  let conStr = dbUrl config
  pool <- PG.init conStr
  let env = AppEnv pool
  run port (app env)
