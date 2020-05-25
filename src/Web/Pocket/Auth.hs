{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Pocket.Auth
  ( AuthRequest(..)
  , makeAuthRequest
  , AuthResponse(..)
  , AuthorizeRequest(..)
  , makeAuthorizeRequest
  , AuthorizeResponse(..)
  )
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
  toJSON authRequest =
    object
      [ "consumer_key" .= authReqConsumerKey authRequest
      , "redirect_uri" .= authReqRedirectUri authRequest
      , "state" .= authReqState authRequest
      ]

makeAuthRequest
  :: Text
  -> Text
  -> AuthRequest
makeAuthRequest authReqConsumerKey authReqRedirectUri =
  AuthRequest { authReqConsumerKey, authReqRedirectUri, authReqState = Nothing }

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
        pure AuthResponse {authRespCode, authRespState}

data AuthorizeRequest =
  AuthorizeRequest
    { authorizeReqConsumerKey :: Text
    , authorizeReqCode :: Text
    }

instance ToJSON AuthorizeRequest where
  toJSON authorizeRequest =
    object
      [ "consumer_key" .= authorizeReqConsumerKey authorizeRequest
      , "code" .= authorizeReqCode authorizeRequest
      ]

makeAuthorizeRequest
  :: Text
  -> Text
  -> AuthorizeRequest
makeAuthorizeRequest authorizeReqConsumerKey authorizeReqCode =
  AuthorizeRequest {authorizeReqCode, authorizeReqConsumerKey}

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
        pure AuthorizeResponse
          { authorizeRespAccessToken
          , authorizeRespState
          , authorizeRespUsername
          }
