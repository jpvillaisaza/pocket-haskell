{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

----------------------------------------------------------------------
-- |
-- Module: Web.Pocket.Add
-- Description:
--
--
--
----------------------------------------------------------------------

module Web.Pocket.Add
  ( AddRequest (..)
  , makeAddRequest
  , AddResponse (..)
  )
  where

-- aeson
import Data.Aeson

-- text
import Data.Text (Text)


-- |
--
--

data AddRequest =
  AddRequest
    { adrConsumerKey :: Text
    , adrAccessToken :: Text
    , adrUrl :: Text
    , adrTitle :: Maybe Text
    , adrTags :: Maybe Text
    , adrTweetId :: Maybe Text
    }


-- |
--
--

makeAddRequest
  :: Text
  -> Text
  -> Text
  -> AddRequest
makeAddRequest adrConsumerKey adrAccessToken adrUrl =
  AddRequest
    { adrTitle = Nothing
    , adrTags = Nothing
    , adrTweetId = Nothing
    , ..
    }


-- |
--
--

instance ToJSON AddRequest where
  toJSON AddRequest {..} =
    object
      [ "url" .= adrUrl
      , "consumer_key" .= adrConsumerKey
      , "access_token" .= adrAccessToken
      ]


-- |
--
--

data AddResponse =
  AddResponse
    { addResponseItem :: Object
    , addResponseStatus :: Integer
    }


-- |
--
--

instance FromJSON AddResponse where
  parseJSON =
    withObject "" $
      \o -> do
        addResponseItem <- o .: "item"
        addResponseStatus <- o .: "status"
        return AddResponse {..}
