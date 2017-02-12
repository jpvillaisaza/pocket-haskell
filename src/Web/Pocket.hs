{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

----------------------------------------------------------------------
-- |
-- Module: Web.Pocket
-- Description:
--
--
--
----------------------------------------------------------------------

module Web.Pocket
  ( AuthRequest (..)
  , makeAuthRequest
  , AuthorizeRequest (..)
  , makeAuthorizeRequest
  , authRequest
  , authorize
  , add
  , AddRequest (..)
  , makeAddRequest
  , AddResponse (..)
  , send
  , Action (..)
  , SendRequest (..)
  , SendResponse (..)
  , get
  , GetRequest (..)
  , makeGetRequest
  , GetResponse (..)
  , run
  )
  where

-- aeson
import Data.Aeson

-- base
import Data.Proxy (Proxy (..))

-- http-client
import Network.HTTP.Client (Manager)

-- servant
import Servant.API

-- servant-client
import Servant.Client hiding (Client)

-- text
import Data.Text (Text)

-- transformers
import Control.Monad.IO.Class


-- |
--
--

type Api =
  "v3"
    :>
      ( "oauth"
          :>
            ( "request"
                :> ReqBody '[JSON] AuthRequest
                :> Post '[JSON] AuthResponse
            :<|>
              "authorize"
                :> ReqBody '[JSON] AuthorizeRequest
                :> Post '[JSON] AuthorizeResponse
            )
      :<|>
        "add"
          :> ReqBody '[JSON] AddRequest
          :> Post '[JSON] AddResponse
      :<|>
        "send"
          :> ReqBody '[JSON] SendRequest
          :> Post '[JSON] SendResponse
      :<|>
        "get"
          :> ReqBody '[JSON] GetRequest
          :> Post '[JSON] GetResponse
      )


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
    withObject "" $ do
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


-- |
--
--

data SendRequest =
  SendRequest
    { srConsumerKey :: Text
    , srAccessToken :: Text
    , srActions :: [Action]
    }


-- |
--
--

instance ToJSON SendRequest where
  toJSON SendRequest {..} =
    object
      [ "consumer_key" .= srConsumerKey
      , "access_token" .= srAccessToken
      , "actions" .= srActions
      ]


-- |
--
--

data Action
  = Add
      { itemId :: Text
      , refId :: Maybe Text
      , tags :: Text -- should be Maybe Text
      , time :: Maybe Text
      , title :: Maybe Text
      , url :: Maybe Text
      }
  | Archive
      { itemId :: Text
      , time :: Maybe Text
      }
  | Delete
      { itemId :: Text
      , time :: Maybe Text
      }
  | Favorite
      { itemId :: Text
      , time :: Maybe Text
      }
  | Readd
      { itemId :: Text
      , time :: Maybe Text
      }
  | TagRename
      { oldTag :: Text
      , newTag :: Text
      , time :: Maybe Text
      }
  | TagsAdd
      { itemId :: Text
      , tags :: Text
      , time :: Maybe Text
      }
  | TagsClear
      { itemId :: Text
      , time :: Maybe Text
      }
  | TagsRemove
      { itemId :: Text
      , tags :: Text
      , time :: Maybe Text
      }
  | TagsReplace
      { itemId :: Text
      , tags :: Text
      , time :: Maybe Text
      }
  | Unfavorite
      { itemId :: Text
      , time :: Maybe Text
      }


-- |
--
--

instance ToJSON Action where
  toJSON Add {..} =
    object
      [ "action" .= ("add" :: Text)
      , "ref_id" .= refId
      , "tags" .= tags
      , "time" .= time
      , "title" .= title
      , "url" .= url
      ]
  toJSON Archive {..} =
    object
      [ "action" .= ("archive" :: Text)
      , "item_id" .= itemId
      , "time" .= time
      ]
  toJSON Delete {..} =
    object
      [ "action" .= ("delete" :: Text)
      , "item_id" .= itemId
      , "time" .= time
      ]
  toJSON Favorite {..} =
    object
      [ "action" .= ("favorite" :: Text)
      , "item_id" .= itemId
      , "time" .= time
      ]
  toJSON Readd {..} =
    object
      [ "action" .= ("readd" :: Text)
      , "item_id" .= itemId
      , "time" .= time
      ]
  toJSON TagRename {..} =
    object
      [ "action" .= ("tag_rename" :: Text)
      , "old_tag" .= oldTag
      , "new_tag" .= newTag
      , "time" .= time
      ]
  toJSON TagsAdd {..} =
    object
      [ "action" .= ("tags_add" :: Text)
      , "item_id" .= itemId
      , "tags" .= tags
      , "time" .= time
      ]
  toJSON TagsClear {..} =
    object
      [ "action" .= ("tags_clear" :: Text)
      , "item_id" .= itemId
      , "time" .= time
      ]
  toJSON TagsRemove {..} =
    object
      [ "action" .= ("tags_remove" :: Text)
      , "item_id" .= itemId
      , "tags" .= tags
      , "time" .= time
      ]
  toJSON TagsReplace {..} =
    object
      [ "action" .= ("tags_replace" :: Text)
      , "item_id" .= itemId
      , "tags" .= tags
      , "time" .= time
      ]
  toJSON Unfavorite {..} =
    object
      [ "action" .= ("unfavorite" :: Text)
      , "item_id" .= itemId
      , "time" .= time
      ]


-- |
--
--

data SendResponse =
  SendResponse
    { actionResults :: [Bool]
    , status :: Integer
    }
  deriving (Show)


-- |
--
--

instance FromJSON SendResponse where
  parseJSON =
    withObject "" $
      \o -> do
        actionResults <- o .: "action_results"
        status <- o .: "status"
        return SendResponse {..}


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


-- |
--
--

authRequest
  :: AuthRequest
  -> ClientM AuthResponse


-- |
--
--

authorize
  :: AuthorizeRequest
  -> ClientM AuthorizeResponse


-- |
--
--

add
  :: AddRequest
  -> ClientM AddResponse


-- |
--
--

send
  :: SendRequest
  -> ClientM SendResponse


-- |
--
--

get
  :: GetRequest
  -> ClientM GetResponse

(authRequest :<|> authorize) :<|> add :<|> send :<|> get =
  client (Proxy :: Proxy Api)


-- |
--
--

run
  :: MonadIO m
  => Manager
  -> ClientM a
  -> m (Either ServantError a)
run manager =
  let
    baseUrl =
      BaseUrl
        Https
        "getpocket.com"
        443
        ""
  in
    liftIO . flip runClientM (ClientEnv manager baseUrl)
