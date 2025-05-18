{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Feature.Campaign.Service where

import qualified Feature.Campaign.Repo as CampaignRepo
import Feature.Campaign.Types
import Feature.Common.Types (AppEnv)
import RIO

class CampaignService m where
  getCampaign :: CampaignId -> m (Maybe Campaign)

instance CampaignService (RIO AppEnv) where
  getCampaign i = CampaignRepo.getCampaign i
