{-# LANGUAGE OverloadedStrings #-}

----------------------------------------------------------------------
-- |
-- Module: Web.Pocket
-- Description:
--
--
--
----------------------------------------------------------------------

module Web.Pocket
  ( authReq
  , authGet
  , authorizeReq
  , add
  , get
  , send
  )
  where

-- base
import Control.Monad.IO.Class (MonadIO)

-- exceptions
import Control.Monad.Catch (MonadThrow)

-- pocket
import Web.Pocket.Add
import Web.Pocket.Auth
import Web.Pocket.Get
import Web.Pocket.Internal
import Web.Pocket.Send

-- text
import qualified Data.Text as Text


authReq :: (MonadIO m, MonadThrow m) => AuthRequest -> m AuthResponse
authReq =
  common "POST https://getpocket.com/v3/oauth/request"

authGet :: AuthRequest -> IO String
authGet ar = do
  authRsp <- authReq ar
  pure $
    "https://getpocket.com/auth/authorize?request_token="
      <> Text.unpack (authRespCode authRsp)
      <> "&redirect_uri="
      <> Text.unpack (authReqRedirectUri ar)

authorizeReq :: (MonadIO m, MonadThrow m) => AuthorizeRequest -> m AuthorizeResponse
authorizeReq =
  common "POST https://getpocket.com/v3/oauth/authorize"


add :: (MonadIO m, MonadThrow m) => AddRequest -> m AddResponse
add =
  common "POST https://getpocket.com/v3/add"

get :: (MonadIO m, MonadThrow m) => GetRequest -> m GetResponse
get =
  common "POST https://getpocket.com/v3/get"

send :: (MonadIO m, MonadThrow m) => SendRequest -> m SendResponse
send =
  common "POST https://getpocket.com/v3/send"
