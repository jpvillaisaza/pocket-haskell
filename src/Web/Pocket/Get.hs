{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

----------------------------------------------------------------------
-- |
-- Module: Web.Pocket.Get
-- Description:
--
--
--
----------------------------------------------------------------------

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


-- |
--
--

data GetRequest =
  GetRequest
    { consumerKey :: Text
    , accessToken :: Text
    , grState ::  Maybe Text
    , grFavorite :: Maybe Integer
    , grTag :: Maybe Text
    , grContentType :: Maybe Text
    , grSort :: Maybe Text
    , grDetailType :: Maybe Text
    , grSearch :: Maybe Text
    , grDomain :: Maybe Text
    , grSince :: Maybe Text
    , grCount :: Maybe Integer
    , grOffset :: Maybe Integer
    }


-- |
--
--

instance ToJSON GetRequest where
  toJSON GetRequest {..} =
    object
      [ "consumer_key" .= consumerKey
      , "access_token" .= accessToken
      , "state" .= grState
      , "favorite" .= grFavorite
      , "tag" .= grTag
      , "contentType" .= grContentType
      , "sort" .= grSort
      , "detailType" .= grDetailType
      , "search" .= grSearch
      , "domain" .= grDomain
      , "since" .= grSince
      , "count" .= grCount
      , "offset" .= grOffset
      ]


-- |
--
--

makeGetRequest
  :: Text
  -> Text
  -> GetRequest
makeGetRequest consumerKey accessToken =
  GetRequest
    consumerKey
    accessToken
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing


-- |
--
--

data GetResponse =
  GetResponse
    { list :: Object
    , getResponseStatus :: Integer
    }
  deriving (Show)


-- |
--
--

instance FromJSON GetResponse where
  parseJSON =
    withObject "" $
      \o -> do
        list <- o .: "list"
        getResponseStatus <- o .: "status"
        return GetResponse {..}
