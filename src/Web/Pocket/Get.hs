{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Web.Pocket.Get
  ( GetRequest (..)
  , makeGetRequest
  , GetResponse (..)
  )
  where

-- aeson
import Data.Aeson

-- text
import Data.Text (Text)


data GetRequest =
  GetRequest
    { getReqConsumerKey :: Text
    , getReqAccessToken :: Text
    , getReqState ::  Maybe Text
    , getReqFavorite :: Maybe Integer
    , getReqTag :: Maybe Text
    , getReqContentType :: Maybe Text
    , getReqSort :: Maybe Text
    , getReqDetailType :: Maybe Text
    , getReqSearch :: Maybe Text
    , getReqDomain :: Maybe Text
    , getReqSince :: Maybe Text
    , getReqCount :: Maybe Integer
    , getReqOffset :: Maybe Integer
    }

instance ToJSON GetRequest where
  toJSON GetRequest {..} =
    object
      [ "consumer_key" .= getReqConsumerKey
      , "access_token" .= getReqAccessToken
      , "state" .= getReqState
      , "favorite" .= getReqFavorite
      , "tag" .= getReqTag
      , "contentType" .= getReqContentType
      , "sort" .= getReqSort
      , "detailType" .= getReqDetailType
      , "search" .= getReqSearch
      , "domain" .= getReqDomain
      , "since" .= getReqSince
      , "count" .= getReqCount
      , "offset" .= getReqOffset
      ]

makeGetRequest
  :: Text
  -> Text
  -> GetRequest
makeGetRequest getReqConsumerKey getReqAccessToken =
  GetRequest
    { getReqState = Nothing
    , getReqFavorite = Nothing
    , getReqTag = Nothing
    , getReqContentType = Nothing
    , getReqSort = Nothing
    , getReqDetailType = Nothing
    , getReqSearch = Nothing
    , getReqDomain = Nothing
    , getReqSince = Nothing
    , getReqCount = Nothing
    , getReqOffset = Nothing
    , ..
    }

data GetResponse =
  GetResponse
    { getRespList :: Object
    , getRespStatus :: Integer
    }
  deriving (Show)

instance FromJSON GetResponse where
  parseJSON =
    withObject "" $
      \o -> do
        getRespList <- o .: "list"
        getRespStatus <- o .: "status"
        return GetResponse {..}
