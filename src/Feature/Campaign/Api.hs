{-# LANGUAGE TypeOperators #-}

module Feature.Campaign.Api (API, server) where

import Feature.Campaign.Service
import Feature.Campaign.Types
import Feature.Common.Types (AppEnv)
import RIO
import Servant

type API =
  "api"
    :> "v1"
    :> "campaigns"
    :> Capture "id" CampaignId
    :> Get '[JSON] Campaign

server :: ServerT API (RIO AppEnv)
server = getCampaign'
  where
    getCampaign' :: CampaignId -> RIO AppEnv Campaign
    getCampaign' i = do
      maybeCampaign <- getCampaign i
      case maybeCampaign of
        Just task -> pure task
        Nothing -> throwM $ NotFound i
