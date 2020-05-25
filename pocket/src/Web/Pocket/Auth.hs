{-# LANGUAGE OverloadedStrings #-}

module Web.Pocket.Auth
  ( AuthRequestReq(..)
  , AuthRequestRsp(..)
  , AuthAuthorizeReq(..)
  , AuthAuthorizeRsp(..)
  )
  where

-- aeson
import Data.Aeson ((.:), (.:?), (.=), FromJSON(..), ToJSON(..))
import qualified Data.Aeson as Aeson

-- text
import Data.Text (Text)


data AuthRequestReq =
  AuthRequestReq
    { authRequestReqConsumerKey :: Text
    , authRequestReqRedirectUri :: Text
    , authRequestReqState :: Maybe Text
    }

instance ToJSON AuthRequestReq where
  toJSON authRequestReq =
    Aeson.object
      [ "consumer_key" .= authRequestReqConsumerKey authRequestReq
      , "redirect_uri" .= authRequestReqRedirectUri authRequestReq
      , "state" .= authRequestReqState authRequestReq
      ]

data AuthRequestRsp =
  AuthRequestRsp
    { authRequestRspCode :: Text
    , authRequestRspState :: Maybe Text
    }

instance FromJSON AuthRequestRsp where
  parseJSON =
    Aeson.withObject "" $
      \o ->
        AuthRequestRsp
          <$> o .: "code"
          <*> o .:? "state"

data AuthAuthorizeReq =
  AuthAuthorizeReq
    { authAuthorizeReqConsumerKey :: Text
    , authAuthorizeReqCode :: Text
    }

instance ToJSON AuthAuthorizeReq where
  toJSON authAuthorizeReq =
    Aeson.object
      [ "consumer_key" .= authAuthorizeReqConsumerKey authAuthorizeReq
      , "code" .= authAuthorizeReqCode authAuthorizeReq
      ]

data AuthAuthorizeRsp =
  AuthAuthorizeRsp
    { authAuthorizeRspAccessToken :: Text
    , authAuthorizeRspUsername :: Text
    , authAuthorizeRspState :: Maybe Text
    }

instance FromJSON AuthAuthorizeRsp where
  parseJSON =
    Aeson.withObject "" $
      \o ->
        AuthAuthorizeRsp
          <$> o .: "access_token"
          <*> o .: "username"
          <*> o .:? "state"
