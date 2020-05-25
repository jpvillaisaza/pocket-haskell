{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Web.Pocket.Send
  ( SendReq(..)
  , Action(..)
  , SendRsp(..)
  )
  where

-- aeson
import Data.Aeson

-- text
import Data.Text (Text)


data SendReq =
  SendReq
    { sendReqConsumerKey :: Text
    , sendReqAccessToken :: Text
    , sendReqActions :: [Action]
    }

instance ToJSON SendReq where
  toJSON sendReq =
    object
      [ "consumer_key" .= sendReqConsumerKey sendReq
      , "access_token" .= sendReqAccessToken sendReq
      , "actions" .= sendReqActions sendReq
      ]

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

data SendRsp =
  SendRsp
    { sendRspActionResults :: [Bool]
    , sendRspStatus :: Integer
    }
  deriving (Show)

instance FromJSON SendRsp where
  parseJSON =
    withObject "" $
      \o ->
        SendRsp
          <$> o .: "action_results"
          <*> o .: "status"
