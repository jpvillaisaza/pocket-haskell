{-# LANGUAGE OverloadedStrings #-}

module Web.Pocket.Add
  ( AddReq(..)
  , makeAddReq
  , AddRsp(..)
  )
  where

-- aeson
import Data.Aeson

-- text
import Data.Text (Text)


data AddReq =
  AddReq
    { addReqConsumerKey :: Text
    , addReqAccessToken :: Text
    , addReqUrl :: Text
    , addReqTitle :: Maybe Text
    , addReqTags :: Maybe Text
    , addReqTweetId :: Maybe Text
    }

makeAddReq
  :: Text
  -> Text
  -> Text
  -> AddReq
makeAddReq consumerKey accessToken url =
  AddReq
    { addReqConsumerKey = consumerKey
    , addReqAccessToken = accessToken
    , addReqUrl = url
    , addReqTitle = Nothing
    , addReqTags = Nothing
    , addReqTweetId = Nothing
    }

instance ToJSON AddReq where
  toJSON addReq =
    object
      [ "url" .= addReqUrl addReq
      , "consumer_key" .= addReqConsumerKey addReq
      , "access_token" .= addReqAccessToken addReq
      ]

data AddRsp =
  AddRsp
    { addRspItem :: Object
    , addRspStatus :: Integer
    }
  deriving (Show)

instance FromJSON AddRsp where
  parseJSON =
    withObject "" $
      \o ->
        AddRsp
          <$> o .: "item"
          <*> o .: "status"
