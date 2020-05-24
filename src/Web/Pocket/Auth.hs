{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Web.Pocket.Auth
  where

-- aeson
import Data.Aeson

-- text
import Data.Text (Text)


data AuthRequest =
  AuthRequest
    { authReqConsumerKey :: Text
    , authReqRedirectUri :: Text
    , authReqState :: Maybe Text
    }


instance ToJSON AuthRequest where
  toJSON AuthRequest {..} =
    object
      [ "consumer_key" .= authReqConsumerKey
      , "redirect_uri" .= authReqRedirectUri
      , "state" .= authReqState
      ]

makeAuthRequest
  :: Text
  -> Text
  -> AuthRequest
makeAuthRequest authReqConsumerKey authReqRedirectUri =
  AuthRequest { authReqState = Nothing, .. }

data AuthResponse =
  AuthResponse
    { authRespCode :: Text
    , authRespState :: Maybe Text
    }
  deriving (Show)

instance FromJSON AuthResponse where
  parseJSON =
    withObject "" $
      \o -> do
        authRespCode <- o .: "code"
        authRespState <- o .: "state"
        return AuthResponse {..}

data AuthorizeRequest =
  AuthorizeRequest
    { authorizeReqConsumerKey :: Text
    , authorizeReqCode :: Text
    }

instance ToJSON AuthorizeRequest where
  toJSON AuthorizeRequest {..} =
    object
      [ "consumer_key" .= authorizeReqConsumerKey
      , "code" .= authorizeReqCode
      ]

makeAuthorizeRequest
  :: Text
  -> Text
  -> AuthorizeRequest
makeAuthorizeRequest authorizeReqConsumerKey authorizeReqCode =
  AuthorizeRequest {..}

data AuthorizeResponse =
  AuthorizeResponse
    { authorizeRespAccessToken :: Text
    , authorizeRespUsername :: Text
    , authorizeRespState :: Maybe Text
    }

instance FromJSON AuthorizeResponse where
  parseJSON =
    withObject "" $
      \o -> do
        authorizeRespAccessToken <- o .: "access_token"
        authorizeRespUsername <- o .: "username"
        authorizeRespState <- o .:? "state"
        return AuthorizeResponse {..}
