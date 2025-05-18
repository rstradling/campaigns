{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Feature.Campaign.Repo where

import Data.Pool (withResource)
import Database.Beam
import Database.Beam.Postgres
import Feature.Campaign.Types
import Feature.Common.Types (HasPgPool, pgPoolL)
import RIO

data DbCampaignT f
  = DbCampaignT
  { _dbCampaignId :: Columnar f Int64,
    _dbCampaignName :: Columnar f Text
  }
  deriving (Generic)

instance Beamable DbCampaignT

instance Table DbCampaignT where
  data PrimaryKey DbCampaignT f = DbCampaignId (Columnar f Int64)
    deriving (Generic, Beamable)
  primaryKey = DbCampaignId . _dbCampaignId

type DbCampaign = DbCampaignT Identity

deriving instance Show DbCampaign

deriving instance Eq DbCampaign

data CampaignsDb f = CampaignsDb
  {_campaigns :: f (TableEntity DbCampaignT)}
  deriving (Generic)

deriving instance Database be CampaignsDb

campaignsDb :: DatabaseSettings be CampaignsDb
campaignsDb = defaultDbSettings

class (Monad m) => CampaignRepo m where
  getCampaign :: CampaignId -> m (Maybe Campaign)

instance (HasPgPool env) => CampaignRepo (RIO env) where
  getCampaign i = do
    poolCon <- view pgPoolL
    let id64 = getCampaignId i
    result <- liftIO $ withResource poolCon $ \con ->
      runBeamPostgres con
        $ runSelectReturningList
        $ select
        $ do
          task <- all_ (_campaigns campaignsDb)
          guard_ (_dbCampaignId task ==. val_ id64)
          pure task
    return $ case result of
      (dbCampaign : _) -> Just $ dbCampaignToDomainCampaign dbCampaign
      [] -> Nothing

dbCampaignToDomainCampaign :: DbCampaign -> Campaign
dbCampaignToDomainCampaign dbCampaign =
  Campaign
    { campaignId = CampaignId (_dbCampaignId dbCampaign),
      campaignName = _dbCampaignName dbCampaign
    }
