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
    { addReqConsumerKey :: Text
    , addReqAccessToken :: Text
    , addReqUrl :: Text
    , addReqTitle :: Maybe Text
    , addReqTags :: Maybe Text
    , addReqTweetId :: Maybe Text
    }


-- |
--
--

makeAddRequest
  :: Text
  -> Text
  -> Text
  -> AddRequest
makeAddRequest addReqConsumerKey addReqAccessToken addReqUrl =
  AddRequest
    { addReqTitle = Nothing
    , addReqTags = Nothing
    , addReqTweetId = Nothing
    , ..
    }


-- |
--
--

instance ToJSON AddRequest where
  toJSON AddRequest {..} =
    object
      [ "url" .= addReqUrl
      , "consumer_key" .= addReqConsumerKey
      , "access_token" .= addReqAccessToken
      ]


-- |
--
--

data AddResponse =
  AddResponse
    { addRespItem :: Object
    , addRespStatus :: Integer
    }


-- |
--
--

instance FromJSON AddResponse where
  parseJSON =
    withObject "" $
      \o -> do
        addRespItem <- o .: "item"
        addRespStatus <- o .: "status"
        return AddResponse {..}
