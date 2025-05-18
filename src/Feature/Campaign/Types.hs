{-# LANGUAGE DeriveAnyClass #-}

module Feature.Campaign.Types where

import Data.Aeson
import RIO
import qualified RIO.Text as Text
import Servant (FromHttpApiData (..), ToHttpApiData (..))

newtype CampaignId = CampaignId Int64 deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance FromHttpApiData CampaignId where
  parseUrlPiece txt = CampaignId <$> parseUrlPiece txt
  parseQueryParam txt = CampaignId <$> parseQueryParam txt

instance ToHttpApiData CampaignId where
  toUrlPiece (CampaignId i) = Text.pack (show i)

getCampaignId :: CampaignId -> Int64
getCampaignId (CampaignId i) = i

data Campaign = Campaign
  { campaignId :: CampaignId,
    campaignName :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data CampaignError = CampaignErrorNotFound CampaignId deriving (Eq, Show, Generic, ToJSON, FromJSON)

data CampaignHttpError = NotFound CampaignId deriving (Typeable)

instance Show CampaignHttpError where
  show (NotFound campaignId') = "Could not find the task with id: " ++ show campaignId'

instance Exception CampaignHttpError
