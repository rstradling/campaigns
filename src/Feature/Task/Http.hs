module Feature.Task.Http
  ( routes,
  )
where

import Data.Aeson (object, (.=))
import Feature.Task.Types
import Network.HTTP.Types.Status
import Platform.Services
import RIO
import Web.Scotty.Trans

routes :: (Service m, MonadUnliftIO m) => ScottyT m ()
routes = do
  get "/api/v1/tasks/:id" $ do
    identifier <- pathParam "id"
    task <- lift $ getTask identifier
    viewTask task
  get "/api/v1/tasks/" $ do
    tasks <- lift getAll
    json tasks

viewTask :: (MonadUnliftIO m) => Maybe Task -> ActionT m ()
viewTask Nothing = do
  status status404
  json $ object ["error" .= ("Task not found" :: String)]
viewTask (Just task) = json task

{-taskErrorHandler :: (ScottyError e, Monad m) => TaskError -> ActionT e m ()
taskErrorHandler = case err of
  TaskErrorNotFound _ -> do
    status status404
    json err
-}
