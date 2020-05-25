{-# LANGUAGE OverloadedStrings #-}

module Web.Pocket.Get
  ( GetReq(..)
  , makeGetReq
  , GetRsp(..)
  )
  where

-- aeson
import Data.Aeson

-- text
import Data.Text (Text)


data GetReq =
  GetReq
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

instance ToJSON GetReq where
  toJSON getReq =
    object
      [ "consumer_key" .= getReqConsumerKey getReq
      , "access_token" .= getReqAccessToken getReq
      , "state" .= getReqState getReq
      , "favorite" .= getReqFavorite getReq
      , "tag" .= getReqTag getReq
      , "contentType" .= getReqContentType getReq
      , "sort" .= getReqSort getReq
      , "detailType" .= getReqDetailType getReq
      , "search" .= getReqSearch getReq
      , "domain" .= getReqDomain getReq
      , "since" .= getReqSince getReq
      , "count" .= getReqCount getReq
      , "offset" .= getReqOffset getReq
      ]

makeGetReq
  :: Text
  -> Text
  -> GetReq
makeGetReq consumerKey accessToken =
  GetReq
    { getReqConsumerKey = consumerKey
    , getReqAccessToken = accessToken
    , getReqState = Nothing
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
    }

data GetRsp =
  GetRsp
    { getRespList :: Object
    , getRespStatus :: Integer
    }
  deriving (Show)

instance FromJSON GetRsp where
  parseJSON =
    withObject "" $
      \o ->
        GetRsp
          <$> o .: "list"
          <*> o .: "status"
