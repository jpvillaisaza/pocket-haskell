{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

----------------------------------------------------------------------
-- |
-- Module: Web.Pocket.Auth
-- Description:
--
--
--
----------------------------------------------------------------------

module Web.Pocket.Auth
  ( AuthRequest (..)
  , makeAuthRequest
  , AuthResponse (..)
  , AuthorizeRequest (..)
  , makeAuthorizeRequest
  , AuthorizeResponse (..)
  )
  where

-- aeson
import Data.Aeson

-- text
import Data.Text (Text)


-- |
--
--

data AuthRequest =
  AuthRequest
    { arConsumerKey :: Text
    , arRedirectUri :: Text
    , arState :: Maybe Text
    }


-- |
--
--

instance ToJSON AuthRequest where
  toJSON AuthRequest {..} =
    object
      [ "consumer_key" .= arConsumerKey
      , "redirect_uri" .= arRedirectUri
      , "state" .= arState
      ]


-- |
--
--

makeAuthRequest
  :: Text
  -> Text
  -> AuthRequest
makeAuthRequest arConsumerKey arRedirectUri =
  AuthRequest { arState = Nothing, .. }


-- |
--
--

data AuthResponse =
  AuthResponse
    { code :: Text
    , state :: Maybe Text
    }
  deriving (Show)


-- |
--
--

instance FromJSON AuthResponse where
  parseJSON =
    withObject "" $
      \o -> do
        code <- o .: "code"
        state <- o .: "state"
        return AuthResponse {..}


-- |
--
--

data AuthorizeRequest =
  AuthorizeRequest
    { arrConsumerKey :: Text
    , arrCode :: Text
    }


-- |
--
--

instance ToJSON AuthorizeRequest where
  toJSON AuthorizeRequest {..} =
    object
      [ "consumer_key" .= arrConsumerKey
      , "code" .= arrCode
      ]


-- |
--
--

makeAuthorizeRequest
  :: Text
  -> Text
  -> AuthorizeRequest
makeAuthorizeRequest arrConsumerKey arrCode =
  AuthorizeRequest {..}


-- |
--
--

data AuthorizeResponse =
  AuthorizeResponse
    { arespAccessToken :: Text
    , arespUsername :: Text
    , arespState :: Maybe Text
    }


-- |
--
--

instance FromJSON AuthorizeResponse where
  parseJSON =
    withObject "" $
      \o -> do
        arespAccessToken <- o .: "access_token"
        arespUsername <- o .: "username"
        arespState <- o .: "state"
        return AuthorizeResponse {..}
