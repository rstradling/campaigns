module Feature.Task.Http
  ( routes,
    Service (..),
  )
where

import Feature.Task.Types
import Network.HTTP.Types.Status
import RIO
import Web.Scotty.Trans

class (Monad m) => Service m where
  getUser :: Int64 -> m (Maybe DM.Task)
  deleteUser :: Int64 -> m (Maybe ())
  getAll :: m [DM.Task]
  update :: DM.Task -> m (Maybe DM.Task)
  create :: DM.Task -> m (Maybe DM.Task)

routes :: (Service m, MonadIO m) -> ScottyT LazyText m ()
routes = do
  get "/api/v1/tasks/:id" $ do
    taskId <- pathParam "id"
    task <- liftIO $ repositoryGet con (createTaskId taskId)
    viewTask task
  get "/api/v1/tasks/" $ do
    tasks <- liftIO $ repositoryGetAll con
    tasksList tasks

taskErrorHandler :: (ScottyError e, Monad m) => TaskError -> ActionT e m ()
taskErrorHandler = case err of
  DM.TaskErrorNotFound _ -> do
    status status404
    json err
