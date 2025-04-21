module Platform.App where

import Platform.Config
import RIO

data MyApp = MyApp
  { myAppConfig :: !AppConfig,
    myAppLogFunc :: !LogFunc
    --    myAppProcessContext :,: !ProcessContext
  }

instance HasLogFunc MyApp where
  logFuncL = lens myAppLogFunc (\x y -> x {myAppLogFunc = y})

-- instance HasProcessContext MyApp where
--  processContextL = lens myAppProcessContext (\x y -> x {myAppProcessContext = y})

class HasAppConfig a where
  myAppConfigL :: Lens' a AppConfig

instance HasAppConfig MyApp where
  myAppConfigL = lens myAppConfig (\x y -> x {myAppConfig = y})
